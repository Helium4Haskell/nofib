#[macro_use]
extern crate criterion;
#[macro_use]
extern crate lazy_static;

use criterion::Criterion;

use regex::Regex;

use chrono::{DateTime, offset::Utc};

use std::sync::Mutex;
use std::collections::HashMap;
use std::process::{Command,Stdio};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::{fs, fs::File};
use std::str;
use std::path::PathBuf;
use std::time::SystemTime;

const INPUT0:   &[u8] = &[0];
const INPUTS5:  &[u8] = &[0,1,2,3,4,5];
const INPUTS9:  &[u8] = &[0,1,2,3,4,5,6,7,8,9];
const INPUTS10: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10];
const INPUTS14: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14];
const INPUTS15: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
const INPUTS25: &[u8] = &[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25];

const NOSEGFAULT: Option<u8> = None;
const SEGFAULTAT7: Option<u8> = Some(7);

const NOFIBS:   &[(&str,&[u8],Option<u8>)] = &
    // "Complex"//LIBS
    // "Debug"  //LIBS
    // "Ratio"  //LIBS
    [("EchoInt",INPUT0,NOSEGFAULT)
        //reads line parses to int returns int Tests call delay
    ,("EchoFloat",INPUT0,NOSEGFAULT)
        //reads line parses to float returns int Tests call delay
    ,("Bernoulli",INPUTS15,NOSEGFAULT)
    ,("DigitsOfE1",INPUTS10,NOSEGFAULT)
    ,("DigitsOfE2",INPUTS10,NOSEGFAULT)
    ,("Exp3_8",INPUTS10,SEGFAULTAT7)
    //,("GenRegexps",INPUTS10)//Generate RegExps
    ,("Integrate",INPUTS5,NOSEGFAULT)
        //use 5000... -> uses 5 * 1000
    ,("Nfib",INPUTS25,NOSEGFAULT)

    ,("Paraffins",INPUTS14,NOSEGFAULT)

    ,("Primes",INPUTS15,NOSEGFAULT)
        //use 1500... -> uses 15 * 100
    ,("Queens",INPUTS9,NOSEGFAULT)
         //measuring past 9 takes an hour
    //,("Tak",INPUTS10,NOSEGFAULT)
        //uses 3 numbers [27 16 8]
    ,("WheelSieve1",INPUTS5,NOSEGFAULT)
        //use 5000... -> uses 5 * 1000
    ,("WheelSieve2",INPUTS5,NOSEGFAULT)
        //use 5000... -> uses 5 * 1000
    ,("X2n1",INPUTS5,NOSEGFAULT)
        //use 5000... -> uses 5 * 1000
    ];

fn criterion_benchmark(c: &mut Criterion) {
    benchmark(c, false);
    benchmark(c, true);
    write_mem_info();
}
fn compile(disable_simplify: bool) {
    for &(nofib,_,_) in NOFIBS {
        // Compiling the programs
        let haskell_file = format!("{}.hs", &nofib);
        let mut command = Command::new("helium");
        command.arg(&haskell_file);
        command.arg("-CB");
        if disable_simplify {
            command.arg("--disable-simplify");
        }
        let output = command.output()
            .expect("running \"helium\" failed!");
        if !output.status.success() {
            eprintln!("{}", nofib);
            dbg!(output);
            panic!("output status unsuccessful!")
        }
    }
}
fn disable_simplify_str(b: bool) -> &'static str {
    match b {
        true => "disable_simplify",
        false => "simplify",
    }
}
fn benchmark(c: &mut Criterion, disable_simplify: bool) {
    compile(disable_simplify);

    let disable_simplify_txt = disable_simplify_str(disable_simplify);
    for &(nofib,inputs,segfault) in NOFIBS {
        // limit inputs to not segfault on running the tests
        let inputs = inputs.iter()
            .take_while(|input| !disable_simplify || segfault.map(|segfault|
                **input < segfault).unwrap_or(true));

        let simplify_nofib = format!("{}/{}",disable_simplify_txt,nofib);
        c.bench_function_over_inputs(&simplify_nofib, move |b, &&size| {
            // Running the compiled code
            let mut child = Command::new("runhelium")
                .arg(&format!("{}.lvm", nofib))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .expect("running \"runhelium\" failed!");
            // Measuring start memory usage
            let child_id = child.id();
            let child_info = format!("/proc/{}/status", child_id);
            process_mem(disable_simplify, nofib, size, true, &child_info);
            // Communication channels with the program
            let mut writer = BufWriter::new(child.stdin.take().expect("child's stdin"));
            let mut reader = BufReader::new(child.stdout.take().expect("child's stdout"));
            let mut buf = vec![];
            b.iter(|| {
                writeln!(writer,"{}",size).expect("write to child's stdin");
                writer.flush().expect("flush child's stdin"); // Deadlocks otherwise (BufWriter)
                buf.clear();
                match reader.read_until(b'\n', &mut buf) {
                    Ok(..) => match std::str::from_utf8(&buf) {
                        Ok(s) => {
                            s.len()
                        },
                        Err(err) => {
                            eprintln!("");
                            std::io::stderr().lock().write_all(&buf).expect("write buf to stderr");
                            dbg!(&buf);
                            dbg!(err);
                            eprintln!("from_utf8()");
                            0
                        },
                    },
                    Err(err) => {
                        eprintln!("");
                        std::io::stderr().lock().write_all(&buf).expect("write buf to stderr");
                        dbg!(&buf);
                        dbg!(err);
                        eprintln!("read_until()");
                        0
                    },
                }
            });
            // Measuring end memory usage
            process_mem(disable_simplify, nofib, size, false, &child_info);
            // Exit child
            writer.write_all(b"\n").expect("write empty line to indicate closing");
            writer.flush().expect("flush empty line to indicate closing");
            if !child.wait().expect("Child has finished").success() {
                panic!("Child did not exit successfully!")
            }
        }, inputs);
    }
}

/*
Map disable_simplify
    Map nofib
        Map input
            Map before
                Map stat
                    Vec<(mem,size)>
*/
lazy_static! {
    static ref MEM_INFO:
        Mutex<
            HashMap<&'static str //bool // disable simplify
                , HashMap<&'static str // nofib
                    , HashMap<&'static str //u8 // input
                        , HashMap<&'static str //bool // before running
                            , HashMap<String // stat
                                , Vec<(u64,String)>>>>>>> = Mutex::new(HashMap::new());
}

fn process_mem(disable_simplify: bool, nofib: &'static str, input: u8, before_running: bool, childs_info: &str) {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?P<stat>Vm\w+):\s+(?P<mem>\d+)\s(?P<size>\w+)").expect("valid regex");
    }
    for line in fs::read_to_string(&childs_info).expect("read child status").lines() {
        if let Some(captures) = RE.captures(line) {
            MEM_INFO.lock().expect("MEM_INFO is unlocked")
                .entry(disable_simplify_str(disable_simplify))                                  .or_default()
                .entry(nofib)                                                                   .or_default()
                .entry(u8_str(input))                                                           .or_default()
                .entry(before_running_str(before_running))                                      .or_default()
                .entry(captures.name("stat").expect("stat in captures").as_str().to_owned())    .or_default()
                .push(
                    ( captures.name("mem").expect("mem in captures").as_str().parse().expect("mem is string representation of u64")
                    , captures.name("size").expect("size in captures").as_str().to_owned()));
        }
    }
}
fn before_running_str(b: bool) -> &'static str {
    match b {
        true => "before_running",
        false => "after_running",
    }
}
fn u8_str(u: u8) -> &'static str {
    match u {        0 => "0",
        1 => "1",
        2 => "2",
        3 => "3",
        4 => "4",
        5 => "5",
        6 => "6",
        7 => "7",
        8 => "8",
        9 => "9",
        10 => "10",
        11 => "11",
        12 => "12",
        13 => "13",
        14 => "14",
        15 => "15",
        16 => "16",
        17 => "17",
        18 => "18",
        19 => "19",
        20 => "20",
        21 => "21",
        22 => "22",
        23 => "23",
        24 => "24",
        25 => "25",
        26 => "26",
        27 => "27",
        28 => "28",
        29 => "29",
        30 => "30",
        31 => "31",
        32 => "32",
        33 => "33",
        34 => "34",
        35 => "35",
        36 => "36",
        37 => "37",
        38 => "38",
        39 => "39",
        40 => "40",
        41 => "41",
        42 => "42",
        43 => "43",
        44 => "44",
        45 => "45",
        46 => "46",
        47 => "47",
        48 => "48",
        49 => "49",
        50 => "50",
        51 => "51",
        52 => "52",
        53 => "53",
        54 => "54",
        55 => "55",
        56 => "56",
        57 => "57",
        58 => "58",
        59 => "59",
        60 => "60",
        61 => "61",
        62 => "62",
        63 => "63",
        64 => "64",
        65 => "65",
        66 => "66",
        67 => "67",
        68 => "68",
        69 => "69",
        70 => "70",
        71 => "71",
        72 => "72",
        73 => "73",
        74 => "74",
        75 => "75",
        76 => "76",
        77 => "77",
        78 => "78",
        79 => "79",
        80 => "80",
        81 => "81",
        82 => "82",
        83 => "83",
        84 => "84",
        85 => "85",
        86 => "86",
        87 => "87",
        88 => "88",
        89 => "89",
        90 => "90",
        91 => "91",
        92 => "92",
        93 => "93",
        94 => "94",
        95 => "95",
        96 => "96",
        97 => "97",
        98 => "98",
        99 => "99",
        100 => "100",
        101 => "101",
        102 => "102",
        103 => "103",
        104 => "104",
        105 => "105",
        106 => "106",
        107 => "107",
        108 => "108",
        109 => "109",
        110 => "110",
        111 => "111",
        112 => "112",
        113 => "113",
        114 => "114",
        115 => "115",
        116 => "116",
        117 => "117",
        118 => "118",
        119 => "119",
        120 => "120",
        121 => "121",
        122 => "122",
        123 => "123",
        124 => "124",
        125 => "125",
        126 => "126",
        127 => "127",
        128 => "128",
        129 => "129",
        130 => "130",
        131 => "131",
        132 => "132",
        133 => "133",
        134 => "134",
        135 => "135",
        136 => "136",
        137 => "137",
        138 => "138",
        139 => "139",
        140 => "140",
        141 => "141",
        142 => "142",
        143 => "143",
        144 => "144",
        145 => "145",
        146 => "146",
        147 => "147",
        148 => "148",
        149 => "149",
        150 => "150",
        151 => "151",
        152 => "152",
        153 => "153",
        154 => "154",
        155 => "155",
        156 => "156",
        157 => "157",
        158 => "158",
        159 => "159",
        160 => "160",
        161 => "161",
        162 => "162",
        163 => "163",
        164 => "164",
        165 => "165",
        166 => "166",
        167 => "167",
        168 => "168",
        169 => "169",
        170 => "170",
        171 => "171",
        172 => "172",
        173 => "173",
        174 => "174",
        175 => "175",
        176 => "176",
        177 => "177",
        178 => "178",
        179 => "179",
        180 => "180",
        181 => "181",
        182 => "182",
        183 => "183",
        184 => "184",
        185 => "185",
        186 => "186",
        187 => "187",
        188 => "188",
        189 => "189",
        190 => "190",
        191 => "191",
        192 => "192",
        193 => "193",
        194 => "194",
        195 => "195",
        196 => "196",
        197 => "197",
        198 => "198",
        199 => "199",
        200 => "200",
        201 => "201",
        202 => "202",
        203 => "203",
        204 => "204",
        205 => "205",
        206 => "206",
        207 => "207",
        208 => "208",
        209 => "209",
        210 => "210",
        211 => "211",
        212 => "212",
        213 => "213",
        214 => "214",
        215 => "215",
        216 => "216",
        217 => "217",
        218 => "218",
        219 => "219",
        220 => "220",
        221 => "221",
        222 => "222",
        223 => "223",
        224 => "224",
        225 => "225",
        226 => "226",
        227 => "227",
        228 => "228",
        229 => "229",
        230 => "230",
        231 => "231",
        232 => "232",
        233 => "233",
        234 => "234",
        235 => "235",
        236 => "236",
        237 => "237",
        238 => "238",
        239 => "239",
        240 => "240",
        241 => "241",
        242 => "242",
        243 => "243",
        244 => "244",
        245 => "245",
        246 => "246",
        247 => "247",
        248 => "248",
        249 => "249",
        250 => "250",
        251 => "251",
        252 => "252",
        253 => "253",
        254 => "254",
        255 => "255",
    }
}

fn write_mem_info() {
    let dirname = "./mem_bench/";
    let now: DateTime<Utc> = SystemTime::now().into();
    let filename_json = PathBuf::from(&format!("{}{}.mem.json", dirname, now));
    let filename_csv = PathBuf::from(&format!("{}{}.mem.csv", dirname, now));

    let dirname = PathBuf::from(dirname);
    if !dirname.is_dir() {
        fs::create_dir(dirname).expect("created mem_bench dir");
    }
    let mut file_json = File::create(&filename_json).expect("created .mem.json file");
    let mut file_csv = File::create(&filename_csv).expect("created .mem.csv file");

    let mem_info = MEM_INFO.lock().expect("MEM_INFO is unlocked");

    let json = serde_json::to_string(&*mem_info).expect("create json from the data");
    file_json.write_all(json.as_bytes()).expect("write json to file");
    println!("[created:]{:?}", filename_json);

    let mut csv = String::new();
    csv.push_str("SIMPLIFY,NOFIB,INPUT,RUNNING,STATISTIC,MEM,KB\n");
    for (simplify, map) in mem_info.iter() {
        for (nofib, map) in map {
            for (input, map) in map {
                for (running, map) in map {
                    for (statistic, vec) in map {
                        for (mem, kb) in vec {
                            csv.push_str(&format!("{},{},{},{},{},{},{}\n", simplify, nofib, input, running, statistic, mem, kb));
                        }
                    }
                }
            }
        }
    }
    file_csv.write_all(csv.as_bytes()).expect("write csv to file");
    println!("[created:]{:?}", filename_csv);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
