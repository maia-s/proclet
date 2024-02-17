use str_block::str_block;

#[test]
pub fn str_block() {
    assert_eq!(str_block!("test"), "test");
    assert_eq!(
        str_block! {"
    line 1
    line 2
    "},
        "line 1\nline 2\n"
    );
    assert_eq!(
        str_block! {"
    line 1
    line 2
"},
        "line 1\nline 2\n"
    );
    assert_eq!(
        str_block! {" \t
        test
    "},
        "test\n"
    );
    assert_eq!(
        str_block! {"
                line 1

    line 2
        line 3"},
        "            line 1\n\nline 2\n    line 3"
    );
}
