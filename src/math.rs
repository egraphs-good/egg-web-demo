use egg::{rewrite as rw, *};
use ordered_float::NotNan;

pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;

pub type Constant = NotNan<f64>;

define_language! {
    pub enum Math {
        "d" = Diff([Id; 2]),
        "i" = Integral([Id; 2]),

        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "pow" = Pow([Id; 2]),
        "ln" = Ln(Id),
        "sqrt" = Sqrt(Id),

        "sin" = Sin(Id),
        "cos" = Cos(Id),

        Constant(Constant),
        Symbol(Symbol),
    }
}

// You could use egg::AstSize, but this is useful for debugging, since
// it will really try to get rid of the Diff operator
pub struct MathCostFn;
impl egg::CostFunction<Math> for MathCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            Math::Diff(..) => 100,
            Math::Integral(..) => 100,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

#[derive(Default)]
pub struct ConstantFold;
impl Analysis<Math> for ConstantFold {
    type Data = Option<Constant>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        if let (Some(c1), Some(c2)) = (to.as_ref(), from.as_ref()) {
            assert_eq!(c1, c2);
        }
        merge_if_different(to, to.or(from))
    }

    fn make(egraph: &EGraph, enode: &Math) -> Self::Data {
        let x = |i: &Id| egraph[*i].data;
        Some(match enode {
            Math::Constant(c) => *c,
            Math::Add([a, b]) => x(a)? + x(b)?,
            Math::Sub([a, b]) => x(a)? - x(b)?,
            Math::Mul([a, b]) => x(a)? * x(b)?,
            Math::Div([a, b]) if x(b) != Some(0.0.into()) => x(a)? / x(b)?,
            _ => return None,
        })
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        let class = &mut egraph[id];
        if let Some(c) = class.data {
            let added = egraph.add(Math::Constant(c));
            let (id, _did_something) = egraph.union(id, added);
            // to not prune, comment this out
            egraph[id].nodes.retain(|n| n.is_leaf());

            assert!(
                !egraph[id].nodes.is_empty(),
                "empty eclass! {:#?}",
                egraph[id]
            );
            #[cfg(debug_assertions)]
            egraph[id].assert_unique_leaves();
        }
    }
}

fn is_const_or_distinct_var(v: &str, w: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let v = v.parse().unwrap();
    let w = w.parse().unwrap();
    move |egraph, _, subst| {
        egraph.find(subst[v]) != egraph.find(subst[w])
            && egraph[subst[v]]
                .nodes
                .iter()
                .any(|n| matches!(n, Math::Constant(..) | Math::Symbol(..)))
    }
}

fn is_const(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let var = var.parse().unwrap();
    move |egraph, _, subst| {
        egraph[subst[var]]
            .nodes
            .iter()
            .any(|n| matches!(n, Math::Constant(..)))
    }
}

fn is_sym(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let var = var.parse().unwrap();
    move |egraph, _, subst| {
        egraph[subst[var]]
            .nodes
            .iter()
            .any(|n| matches!(n, Math::Symbol(..)))
    }
}

fn is_not_zero(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    let var = var.parse().unwrap();
    let zero = Math::Constant(0.0.into());
    move |egraph, _, subst| !egraph[subst[var]].nodes.contains(&zero)
}

#[rustfmt::skip]
pub fn rules() -> Vec<Rewrite> { vec![
    rw!("comm-add";  "(+ ?a ?b)"        => "(+ ?b ?a)"),
    rw!("comm-mul";  "(* ?a ?b)"        => "(* ?b ?a)"),
    rw!("assoc-add"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)"),
    rw!("assoc-mul"; "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)"),

    rw!("sub-canon"; "(- ?a ?b)" => "(+ ?a (* -1 ?b))"),
    rw!("div-canon"; "(/ ?a ?b)" => "(* ?a (pow ?b -1))" if is_not_zero("?b")),
    // rw!("canon-sub"; "(+ ?a (* -1 ?b))"   => "(- ?a ?b)"),
    // rw!("canon-div"; "(* ?a (pow ?b -1))" => "(/ ?a ?b)" if is_not_zero("?b")),

    rw!("zero-add"; "(+ ?a 0)" => "?a"),
    rw!("zero-mul"; "(* ?a 0)" => "0"),
    rw!("one-mul";  "(* ?a 1)" => "?a"),

    rw!("add-zero"; "?a" => "(+ ?a 0)"),
    rw!("mul-one";  "?a" => "(* ?a 1)"),

    rw!("cancel-sub"; "(- ?a ?a)" => "0"),
    rw!("cancel-div"; "(/ ?a ?a)" => "1" if is_not_zero("?a")),

    rw!("distribute"; "(* ?a (+ ?b ?c))"        => "(+ (* ?a ?b) (* ?a ?c))"),
    rw!("factor"    ; "(+ (* ?a ?b) (* ?a ?c))" => "(* ?a (+ ?b ?c))"),

    rw!("pow-mul"; "(* (pow ?a ?b) (pow ?a ?c))" => "(pow ?a (+ ?b ?c))"),
    rw!("pow0"; "(pow ?x 0)" => "1"
        if is_not_zero("?x")),
    rw!("pow1"; "(pow ?x 1)" => "?x"),
    rw!("pow2"; "(pow ?x 2)" => "(* ?x ?x)"),
    rw!("pow-recip"; "(pow ?x -1)" => "(/ 1 ?x)"
        if is_not_zero("?x")),
    rw!("recip-mul-div"; "(* ?x (/ 1 ?x))" => "1" if is_not_zero("?x")),
]}

egg::test_fn! {
    math_associate_adds, [
        rw!("comm-add"; "(+ ?a ?b)" => "(+ ?b ?a)"),
        rw!("assoc-add"; "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)"),
    ],
    runner = Runner::default()
        .with_iter_limit(7)
        .with_scheduler(SimpleScheduler),
    "(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 7))))))"
    =>
    "(+ 7 (+ 6 (+ 5 (+ 4 (+ 3 (+ 2 1))))))"
    @check |r: Runner<Math, ()>| assert_eq!(r.egraph.number_of_classes(), 127)
}

egg::test_fn! {
    #[should_panic(expected = "Could not prove goal 0")]
    math_fail, rules(),
    "(+ x y)" => "(/ x y)"
}

egg::test_fn! {math_simplify_add, rules(), "(+ x (+ x (+ x x)))" => "(* 4 x)" }
egg::test_fn! {math_powers, rules(), "(* (pow 2 x) (pow 2 y))" => "(pow 2 (+ x y))"}

egg::test_fn! {
    math_simplify_const, rules(),
    "(+ 1 (- a (* (- 2 1) a)))" => "1"
}

egg::test_fn! {
    math_simplify_factor, rules(),
    "(* (+ x 3) (+ x 1))"
    =>
    "(+ (+ (* x x) (* 4 x)) 3)"
}
