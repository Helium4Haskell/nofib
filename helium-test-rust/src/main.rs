use std::process::{Command,Stdio};
use std::{io, io::{BufRead, BufReader, BufWriter, Write, Read}};
use std::str;
use std::{fs, fs::File};
use std::path::PathBuf;

const INPUT0:   &[u8] = &[0];
const INPUTS5:  &[u8] = &[0,1,2,3,4,5];
const INPUTS9:  &[u8] = &[0,1,2,3,4,5,6,7,8,9];
const INPUTS10: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10];
const INPUTS14: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14];
const INPUTS15: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
const INPUTS25: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25];
const NOFIBS:   &[(&str,&[u8])] = &
    // "Complex"//LIBS
    // "Debug"  //LIBS
    // "Ratio"  //LIBS
    [("EchoInt",INPUT0)//reads line parses to int returns int Tests call delay
    ,("EchoFloat",INPUT0)//reads line parses to float returns int Tests call delay
    ,("Bernoulli",INPUTS15)
    ,("DigitsOfE1",INPUTS10)
    ,("DigitsOfE2",INPUTS10)
    ,("Exp3_8",INPUTS10)
    //,("GenRegexps",INPUTS10)//Generate RegExps
    ,("Integrate",INPUTS5)//use 5000... -> uses 5 * 1000
    ,("Nfib",INPUTS25)
    ,("Paraffins",INPUTS14)
    ,("Primes",INPUTS15)//use 1500... -> uses 15 * 100
    ,("Queens",INPUTS9) //measuring past 9 takes an hour
    //,("Tak",INPUTS10)//uses 3 numbers [27 16 8]
    ,("WheelSieve1",INPUTS5)//use 5000... -> uses 5 * 1000
    ,("WheelSieve2",INPUTS5)//use 5000... -> uses 5 * 1000
    ,("X2n1",INPUTS5)//use 5000... -> uses 5 * 1000
    ];

fn main() -> io::Result<()> {
    for &(nofib,_inputs) in NOFIBS {
        // Compiling the programs
        let output = Command::new("helium")
            .arg(&format!("{}.hs", nofib))
            .arg("-CB")
            .output()
            .expect("running \"helium\" failed!");
        if !output.status.success() { eprintln!("{}", nofib); dbg!(output); panic!("output status unsuccessful!") };
    }
    for &(nofib,inputs) in NOFIBS {
        // Running the compiled code
        let child = Command::new("runhelium")
            .arg(&format!("{}.lvm", nofib))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("running \"runhelium\" failed!");
        // Communication channels with the program
        let mut writer = BufWriter::new(child.stdin.expect("child's stdin"));
        let mut reader = BufReader::new(child.stdout.expect("child's stdout"));
        let mut buf = vec![];
        for size in inputs {
            writeln!(writer,"{}",size).expect("write to child's stdin");
            writer.flush().expect("flush child's stdin"); // Deadlocks otherwise (BufWriter)
            buf.clear();
            match reader.read_until(b'\n', &mut buf) {
                Ok(..) => match std::str::from_utf8(&buf) {
                    Ok(s) => {
                        let dirname = "./cmp_outs/";
                        let filename = PathBuf::from(&format!("{}{}.{}.cmp", dirname, nofib, size));
                        let dirname = PathBuf::from(dirname);
                        if filename.is_file() {
                            let mut file = File::open(&filename)?;
                            let mut file_txt = String::new();
                            file.read_to_string(&mut file_txt)?;
                            if file_txt != s {
                                println!("[unequal:]{:?}", filename);
                            } else {
                                println!("[equal:]{:?}", filename);
                            }
                        } else {
                            if !dirname.is_dir() {
                                //println!("[creating:]{:?}", dirname);
                                fs::create_dir(dirname)?;
                            }
                            //println!("[creating:]{:?}", filename);
                            let mut file = File::create(&filename)?;
                            file.write_all(s.as_bytes())?;
                            println!("[created:]{:?}", filename);
                        }
                    },
                    Err(err) => {
                        eprintln!("");
                        std::io::stderr().lock().write_all(&buf).expect("write buf to stderr");
                        dbg!(&buf);
                        dbg!(err);
                        eprintln!("from_utf8()");
                    },
                },
                Err(err) => {
                    eprintln!("");
                    std::io::stderr().lock().write_all(&buf).expect("write buf to stderr");
                    dbg!(&buf);
                    dbg!(err);
                    eprintln!("read_until()");
                },
            }
        }
        writer.write_all(b"\n").expect("write empty line to indicate closing");
        writer.flush().expect("flush empty line to indicate closing");
    }
    Ok(())
}
