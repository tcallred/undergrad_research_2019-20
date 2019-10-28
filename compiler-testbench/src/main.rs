use std::process::{Command, Output};
//use std::fs;

trait OutputHelper {
    fn print_all_output(&self);
}

impl OutputHelper for Output {
    fn print_all_output(&self) {
        print!("{}", String::from_utf8((*self.stdout).to_owned()).expect("Utf8 error"));
    }
}

//fn compile_haskell(path: String) {
//    let stack_build = Command::new("cmd")
//        .args(&["/C", "stack build"])
//        .current_dir(path)
//        .output()
//        .expect("Failed to build haskell");
//    stack_build.print_all_output();
//}


fn main() {
    let cd = Command::new("cmd")
        .args(&["/C", "cd"])
        .output()
        .expect("failed");

    println!("Current dir: ");
    cd.print_all_output();
}
