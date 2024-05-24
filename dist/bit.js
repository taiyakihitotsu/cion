"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// test
const bitor1 = `1`;
const bitor2 = `1`;
const bitor3 = `1`;
const bitor4 = `0`;
const bitor5 = `010`;
const bitor6 = `111`;
const bitor7 = `110`;
const bitor8 = `000`;
// test
const bitand1 = `1`;
const bitand2 = `0`;
const bitand3 = `0`;
const bitand4 = `0`;
const bitand5 = `000`;
const bitand6 = `111`;
const bitand7 = `110`;
const bitand8 = `000`;
// test
const bitxor1 = `0`;
const bitxor2 = `1`;
const bitxor3 = `1`;
const bitxor4 = `0`;
const bitxor5 = `010`;
const bitxor6 = `000`;
const bitxor7 = `000`;
const bitxor8 = `000`;
const bitxor9 = `100`;
const bitxor10 = `00010`;
// test
const bitshiftlg0 = `1110`;
const bitshiftlg1 = `0000`;
const bitshiftlg2 = `0100`;
// test
const bitshiftl0 = `1100`;
const bitshiftl1 = `0000`;
const bitshiftl2 = `0100`;
const bitshiftl3 = `1000`;
const bitshiftl4 = `0000`;
// fixme : should it be an error ?
const bitshiftl5 = `0000`;
// test
const bitnot0 = "1";
const bitnot1 = "0";
const bitnot2 = "00111";
// test
const biteq0 = true;
const biteq1 = false;
const biteq2 = false;
const biteq3 = true;
const biteq4 = false;
const biteq5 = true;
const biteq6 = false;
const biteq7 = false;
const biteq8 = false;
// todo : does init arg need?
const biteq9 = true;
// test
const bitlen0 = [[[[[[[[null]]]]]]]];
// test
const bitgthan0 = false;
const bitgthan1 = false;
const bitgthan2 = true;
// todo : they should be an error.
const bitgthan3 = false;
const bitgthan4 = false;
const bitgthan5 = false;
// test
const bitneedfill0 = [[[[[null]]]]];
// test
const bitpadding0 = "010101";
const bitpadding1 = "0010101";
const bitpadding2 = "1110101";
// test
const bituniform0 = ["01111", "00000"];
const bituniform1 = ["001111", "000000"];
const bituniform2 = ["001111", "111100"];
// test
const bitcut0 = "11111";
const bitcut1 = "1111";
// test
const bitiszero0 = false;
const bitiszero1 = true;
// test
const bitfill0 = "00001111";
const bitfill1 = "00000000";
const bitfill2 = "00000111";
const bitfill3 = "00000011";
const bitfill4 = "00000001";
// fixme : want to spit an error
const bitfill5 = "00000000";
// test
const _bitadd0 = "01100";
const _bitadd1 = "00111";
const _bitadd2 = "00000";
const _bitadd3 = "11110";
// test
// 7,5,12
// 6,1,7
// 0,0,0
// 31,31,62
const bitadd0 = "00001100";
const bitadd1 = "00000111";
const bitadd2 = "00000000";
const bitadd3 = "00111110"; // shift.
// test
const bitsub0 = "00000010";
const bitsub1 = "00000101";
const bitsub2 = "00000000";
const bitsub3 = "00000000";
// test
// 7,5,35
// 6,2,12
// 0,0,0
// 31,1,31
// 1,1,1
// 0,1,0
// 1,0,0
const bitmul0 = "00100011";
const bitmul1 = "00001100";
const bitmul2 = "00000000";
const bitmul3 = "00011111";
const bitmul4 = "00000001";
const bitmul5 = "00000000";
const bitmul6 = "00000000";
const bitsr0 = "1111";
const bitsr1 = "111";
// type BitShiftRight
// type UnsignedBitShiftRight
