#![recursion_limit = "1024"]

use std::convert::TryInto;

use wasm_bindgen::prelude::*;
use egg::{EClass, Id, Pattern, RecExpr, SearchMatches, Searcher};
use yew::{prelude::*, services::ConsoleService};

mod math;
use math::*;

type Extractor<'a> = egg::Extractor<'a, MathCostFn, Math, ConstantFold>;

struct Queried {
    pattern: Pattern<Math>,
    matches: Vec<SearchMatches>,
}

struct Model {
    link: ComponentLink<Self>,
    egraph: EGraph,
    query: Result<Queried, String>,
    added: Vec<Added>,
    examples: Vec<&'static str>,
    rewrites: Vec<OptionalRewrite>,
}

struct OptionalRewrite {
    applied: usize,
    matched: usize,
    enabled: bool,
    rewrite: Rewrite,
}

fn percent(a: usize, b: usize) -> f64 {
    if b == 0 {
        0.0
    } else {
        a as f64 / b as f64 * 100.0
    }
}

impl OptionalRewrite {
    fn new(rewrite: Rewrite) -> Self {
        Self {
            applied: 0,
            matched: 0,
            enabled: true,
            rewrite,
        }
    }
}

impl OptionalRewrite {
    fn render(&self, i: usize, link: &ComponentLink<Model>) -> Html {
        let percent = percent(self.applied, self.matched);
        let counts = format!("{}/{} ({:.0}%)", self.applied, self.matched, percent);
        let onclick = link.callback(move |_| Msg::ToggleRewrite(i));
        html! {
            <div class="rewrite",>
                <input type="checkbox", checked=self.enabled, onclick=onclick, />
                <details>
                    <summary> {counts} {" "} {self.rewrite.name()}</summary>
                    <div class="longname",> {format!("{:?}", self.rewrite)} </div>
                </details>
            </div>
        }
    }
}

struct Added {
    id: Id,
    expr: RecExpr<Math>,
}

impl Renderable for Added {
    fn render(&self) -> Html {
        html! { <tr> <td> {self.expr.pretty(100)} </td> <td> {self.id} </td> </tr> }
    }
}

enum Msg {
    AddExpr(String),
    AddQuery,
    RunRewrites,
    UpdateQuery(String),
    ToggleRewrite(usize),
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        Model {
            link,
            egraph: EGraph::default(),
            query: Err("enter a pattern or expression".into()),
            added: vec![],
            examples: vec!["(+ 1 2)", "(* x (+ y z))", "(+ x (+ x (+ x x)))"],
            rewrites: math::rules()
                .into_iter()
                .map(OptionalRewrite::new)
                .collect(),
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        // Should only return "true" if new properties are different to
        // previously received properties.
        // This component has no properties so we will always return "false".
        false
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::UpdateQuery(s) => {
                self.query = s.parse().map(|pattern: Pattern<_>| {
                    let matches = pattern.search(&self.egraph);
                    Queried { pattern, matches }
                });
            }
            Msg::AddQuery => {
                if let Ok(pat) = &self.query {
                    match pat.pattern.clone().try_into() {
                        Ok(expr) => {
                            let id = self.egraph.add_expr(&expr);
                            self.update(Msg::UpdateQuery(expr.pretty(100)));
                            self.added.push(Added { expr, id });
                        }
                        Err(v) => {
                            self.query = Err(format!("Found var {}", v));
                        }
                    }
                }
            }
            Msg::AddExpr(s) => {
                self.update(Msg::UpdateQuery(s));
                self.update(Msg::AddQuery);
            }
            Msg::ToggleRewrite(i) => {
                self.rewrites[i].enabled ^= true;
            }
            Msg::RunRewrites => {
                ConsoleService::time_named("running rules");
                let mut matches = Vec::new();

                for rule in &mut self.rewrites {
                    if rule.enabled {
                        let ms = rule.rewrite.search(&self.egraph);
                        rule.matched += ms.len();
                        matches.push((&rule.rewrite, &mut rule.applied, ms));
                    }
                }

                for (rule, applied, ms) in matches {
                    let actually_matched = rule.apply(&mut self.egraph, &ms);
                    *applied += actually_matched.len();
                }

                self.egraph.rebuild();

                ConsoleService::time_named_end("running rules");
            }
        };
        true
    }

    fn view(&self) -> Html {
        let mut extract = Extractor::new(&self.egraph, MathCostFn);
        let query_message = match &self.query {
            Ok(q) => {
                let found: Vec<Id> = q.matches.iter().map(|m| m.eclass).collect();
                format!("Found at {:?}", found)
            }
            Err(s) => s.clone(),
        };

        let oninput = self.link.callback(|e: InputData| Msg::UpdateQuery(e.value));
        let onclick = self.link.callback(|_| Msg::RunRewrites);
        let onsubmit = self.link.callback(|e: FocusEvent| {e.prevent_default(); Msg::AddQuery});
        html! {
        <div>
            <section id="add",>
                <form onsubmit=onsubmit>
                    <input oninput=oninput, />
                </form>
                <p> {query_message} </p>
                <table>
                    <tr>
                        <th> {"expr"} </th>
                        <th> {"eclass"} </th>
                    </tr>
                    { for self.added.iter().map(Renderable::render) }
                </table>
            </section>
            <section id="examples",>
                <h3> {"Examples"} </h3>
                <div>
                    { for self.examples.iter().cloned().map(|ex| render_example(ex, &self.link)) }
                </div>
            </section>
            <section id="stats",>
                <h3> {"Stats"} </h3>
                <div> { format!("Nodes: {}", self.egraph.total_size()) } </div>
            </section>
            <section id="eclasses",>
                <h3> {"EClasses"} </h3>
                <div> { for self.egraph.classes().map(|c| render_eclass(&mut extract, c)) } </div>
            </section>
            <section id="rewrites",>
                <h3> {"Rewrites"} </h3>
                <button onclick=onclick,>{"Run"}</button>
                <div>
                    { for self.rewrites.iter().enumerate().map(|(i, r)| r.render(i, &self.link)) }
                </div>
            </section>
        </div>
        }
    }
}

fn render_example(s: &'static str, link: &ComponentLink<Model>) -> Html {
    let onclick = link.callback(move |_| Msg::AddExpr(s.to_string()));
    html! { <div onclick=onclick,> {s} </div> }
}

fn render_eclass(extractor: &mut Extractor, eclass: &EClass<Math, Option<Constant>>) -> Html {
    let (cost, best) = extractor.find_best(eclass.id);
    html! {
        <details class="eclass",>
            <summary> {eclass.id} </summary>
            <p>{format!("Size: {}", eclass.len())}</p>
            <p>{format!("Best: {}", best.pretty(100))}</p>
            <p>{format!("Cost: {}", cost)}</p>
        </details>
    }
}

#[wasm_bindgen(start)]
pub fn run_app() {
    App::<Model>::new().mount_to_body();
}
