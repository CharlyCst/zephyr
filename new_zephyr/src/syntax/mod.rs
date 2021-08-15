mod common;
mod green;
mod red;

#[cfg(test)]
mod test {
    use super::common::*;
    use super::green::*;
    use super::red::*;
    use std::sync::Arc;

    #[test]
    fn smoke() {
        let ws = Arc::new(GreenTokenData::new(SyntaxKind::Whitespace, " ".to_string()));
        let one = Arc::new(GreenTokenData::new(SyntaxKind::Int, "1".to_string()));
        let two = Arc::new(GreenTokenData::new(SyntaxKind::Int, "2".to_string()));
        let three = Arc::new(GreenTokenData::new(SyntaxKind::Int, "3".to_string()));
        let plus = Arc::new(GreenTokenData::new(SyntaxKind::Plus, "+".to_string()));
        let star = Arc::new(GreenTokenData::new(SyntaxKind::Star, "*".to_string()));

        let multiplication = Arc::new(GreenNodeData::new(
            SyntaxKind::BinExpr,
            vec![
                one.into(),
                ws.clone().into(),
                star.into(),
                ws.clone().into(),
                two.into(),
            ],
        ));
        let addition = Arc::new(GreenNodeData::new(
            SyntaxKind::BinExpr,
            vec![
                multiplication.clone().into(),
                ws.clone().into(),
                plus.into(),
                ws.into(),
                multiplication.into(),
            ],
        ));

        // Print the expression
        println!("{}", addition);

        let addition = RedNodeData::new(addition);
        let mul2 = addition.children().nth(4).unwrap().into_node().unwrap();
        let one2 = mul2.children().next().unwrap().into_token().unwrap();

        println!("{} at offset {}", one2, one2.text_offset());
        println!("{}", one2.parent().unwrap().parent().unwrap());

        let three = RedTokenData::new(three);
        let new_mul = mul2.replace_child(0, three.into());
        println!("{}", new_mul.parent().unwrap());
        println!("second mul at {}, len {}", new_mul.text_offset(), new_mul.text_len());
    }
}
