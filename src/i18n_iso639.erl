%% -------------------------------------------------------------------------
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @copyright 2004 Torbjörn Törnkvist
%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%% @author Serge Aleynikov <saleyn@gmail.com>
%% @doc ISO 639 2-letter codes.
%% @private

-module(i18n_iso639).
-export([decode_iso2/1, all_iso2/0]).

decode_iso2(aa) -> <<"Afar">>;
decode_iso2(ab) -> <<"Abkhazian">>;
decode_iso2(ae) -> <<"Avestan">>;
decode_iso2(af) -> <<"Afrikaans">>;
decode_iso2(am) -> <<"Amharic">>;
decode_iso2(ar) -> <<"Arabic">>;
decode_iso2(as) -> <<"Assamese">>;
decode_iso2(ay) -> <<"Aymara">>;
decode_iso2(az) -> <<"Azerbaijani">>;
decode_iso2(ba) -> <<"Bashkir">>;
decode_iso2(be) -> <<"Byelorussian; Belarusian">>;
decode_iso2(bg) -> <<"Bulgarian">>;
decode_iso2(bh) -> <<"Bihari">>;
decode_iso2(bi) -> <<"Bislama">>;
decode_iso2(bn) -> <<"Bengali; Bangla">>;
decode_iso2(bo) -> <<"Tibetan">>;
decode_iso2(br) -> <<"Breton">>;
decode_iso2(bs) -> <<"Bosnian">>;
decode_iso2(ca) -> <<"Catalan">>;
decode_iso2(ce) -> <<"Chechen">>;
decode_iso2(ch) -> <<"Chamorro">>;
decode_iso2(co) -> <<"Corsican">>;
decode_iso2(cs) -> <<"Czech">>;
decode_iso2(cu) -> <<"Church Slavic">>;
decode_iso2(cv) -> <<"Chuvash">>;
decode_iso2(cy) -> <<"Welsh">>;
decode_iso2(da) -> <<"Danish">>;
decode_iso2(de) -> <<"German">>;
decode_iso2(dz) -> <<"Dzongkha; Bhutani">>;
decode_iso2(el) -> <<"Greek">>;
decode_iso2(en) -> <<"English">>;
decode_iso2(eo) -> <<"Esperanto">>;
decode_iso2(es) -> <<"Spanish">>;
decode_iso2(et) -> <<"Estonian">>;
decode_iso2(eu) -> <<"Basque">>;
decode_iso2(fa) -> <<"Persian">>;
decode_iso2(fi) -> <<"Finnish">>;
decode_iso2(fj) -> <<"Fijian; Fiji">>;
decode_iso2(fo) -> <<"Faroese">>;
decode_iso2(fr) -> <<"French">>;
decode_iso2(fy) -> <<"Frisian">>;
decode_iso2(ga) -> <<"Irish">>;
decode_iso2(gd) -> <<"Scots; Gaelic">>;
decode_iso2(gl) -> <<"Gallegan; Galician">>;
decode_iso2(gn) -> <<"Guarani">>;
decode_iso2(gu) -> <<"Gujarati">>;
decode_iso2(gv) -> <<"Manx">>;
decode_iso2(ha) -> <<"Hausa (?)">>;
decode_iso2(he) -> <<"Hebrew (formerly iw)">>;
decode_iso2(hi) -> <<"Hindi">>;
decode_iso2(ho) -> <<"Hiri Motu">>;
decode_iso2(hr) -> <<"Croatian">>;
decode_iso2(hu) -> <<"Hungarian">>;
decode_iso2(hy) -> <<"Armenian">>;
decode_iso2(hz) -> <<"Herero">>;
decode_iso2(ia) -> <<"Interlingua">>;
decode_iso2(id) -> <<"Indonesian (formerly in)">>;
decode_iso2(ie) -> <<"Interlingue">>;
decode_iso2(ik) -> <<"Inupiak">>;
decode_iso2(io) -> <<"Ido">>;
decode_iso2(is) -> <<"Icelandic">>;
decode_iso2(it) -> <<"Italian">>;
decode_iso2(iu) -> <<"Inuktitut">>;
decode_iso2(ja) -> <<"Japanese">>;
decode_iso2(jv) -> <<"Javanese">>;
decode_iso2(ka) -> <<"Georgian">>;
decode_iso2(ki) -> <<"Kikuyu">>;
decode_iso2(kj) -> <<"Kuanyama">>;
decode_iso2(kk) -> <<"Kazakh">>;
decode_iso2(kl) -> <<"Kalaallisut; Greenlandic">>;
decode_iso2(km) -> <<"Khmer; Cambodian">>;
decode_iso2(kn) -> <<"Kannada">>;
decode_iso2(ko) -> <<"Korean">>;
decode_iso2(ks) -> <<"Kashmiri">>;
decode_iso2(ku) -> <<"Kurdish">>;
decode_iso2(kv) -> <<"Komi">>;
decode_iso2(kw) -> <<"Cornish">>;
decode_iso2(ky) -> <<"Kirghiz">>;
decode_iso2(la) -> <<"Latin">>;
decode_iso2(lb) -> <<"Letzeburgesch">>;
decode_iso2(ln) -> <<"Lingala">>;
decode_iso2(lo) -> <<"Lao; Laotian">>;
decode_iso2(lt) -> <<"Lithuanian">>;
decode_iso2(lv) -> <<"Latvian; Lettish">>;
decode_iso2(mg) -> <<"Malagasy">>;
decode_iso2(mh) -> <<"Marshall">>;
decode_iso2(mi) -> <<"Maori">>;
decode_iso2(mk) -> <<"Macedonian">>;
decode_iso2(ml) -> <<"Malayalam">>;
decode_iso2(mn) -> <<"Mongolian">>;
decode_iso2(mo) -> <<"Moldavian">>;
decode_iso2(mr) -> <<"Marathi">>;
decode_iso2(ms) -> <<"Malay">>;
decode_iso2(mt) -> <<"Maltese">>;
decode_iso2(my) -> <<"Burmese">>;
decode_iso2(na) -> <<"Nauru">>;
decode_iso2(nb) -> <<"Norwegian Bokmål">>;
decode_iso2(nd) -> <<"Ndebele, North">>;
decode_iso2(ne) -> <<"Nepali">>;
decode_iso2(ng) -> <<"Ndonga">>;
decode_iso2(nl) -> <<"Dutch">>;
decode_iso2(nn) -> <<"Norwegian Nynorsk">>;
decode_iso2(no) -> <<"Norwegian">>;
decode_iso2(nr) -> <<"Ndebele, South">>;
decode_iso2(nv) -> <<"Navajo">>;
decode_iso2(ny) -> <<"Chichewa; Nyanja">>;
decode_iso2(oc) -> <<"Occitan; Provençal">>;
decode_iso2(om) -> <<"(Afan) Oromo">>;
decode_iso2('or') -> <<"Oriya">>;
decode_iso2(os) -> <<"Ossetian; Ossetic">>;
decode_iso2(pa) -> <<"Panjabi; Punjabi">>;
decode_iso2(pi) -> <<"Pali">>;
decode_iso2(pl) -> <<"Polish">>;
decode_iso2(ps) -> <<"Pashto, Pushto">>;
decode_iso2(pt) -> <<"Portuguese">>;
decode_iso2(qu) -> <<"Quechua">>;
decode_iso2(rm) -> <<"Rhaeto-Romance">>;
decode_iso2(rn) -> <<"Rundi; Kirundi">>;
decode_iso2(ro) -> <<"Romanian">>;
decode_iso2(ru) -> <<"Russian">>;
decode_iso2(rw) -> <<"Kinyarwanda">>;
decode_iso2(sa) -> <<"Sanskrit">>;
decode_iso2(sc) -> <<"Sardinian">>;
decode_iso2(sd) -> <<"Sindhi">>;
decode_iso2(se) -> <<"Northern Sami">>;
decode_iso2(sg) -> <<"Sango; Sangro">>;
decode_iso2(si) -> <<"Sinhalese">>;
decode_iso2(sk) -> <<"Slovak">>;
decode_iso2(sl) -> <<"Slovenian">>;
decode_iso2(sm) -> <<"Samoan">>;
decode_iso2(sn) -> <<"Shona">>;
decode_iso2(so) -> <<"Somali">>;
decode_iso2(sq) -> <<"Albanian">>;
decode_iso2(sr) -> <<"Serbian">>;
decode_iso2(ss) -> <<"Swati; Siswati">>;
decode_iso2(st) -> <<"Sesotho; Sotho, Southern">>;
decode_iso2(su) -> <<"Sundanese">>;
decode_iso2(sv) -> <<"Swedish">>;
decode_iso2(sw) -> <<"Swahili">>;
decode_iso2(ta) -> <<"Tamil">>;
decode_iso2(te) -> <<"Telugu">>;
decode_iso2(tg) -> <<"Tajik">>;
decode_iso2(th) -> <<"Thai">>;
decode_iso2(ti) -> <<"Tigrinya">>;
decode_iso2(tk) -> <<"Turkmen">>;
decode_iso2(tl) -> <<"Tagalog">>;
decode_iso2(tn) -> <<"Tswana; Setswana">>;
decode_iso2(to) -> <<"Tonga (?)">>;
decode_iso2(tr) -> <<"Turkish">>;
decode_iso2(ts) -> <<"Tsonga">>;
decode_iso2(tt) -> <<"Tatar">>;
decode_iso2(tw) -> <<"Twi">>;
decode_iso2(ty) -> <<"Tahitian">>;
decode_iso2(ug) -> <<"Uighur">>;
decode_iso2(uk) -> <<"Ukrainian">>;
decode_iso2(ur) -> <<"Urdu">>;
decode_iso2(uz) -> <<"Uzbek">>;
decode_iso2(vi) -> <<"Vietnamese">>;
decode_iso2(vo) -> <<"Volapuk">>;
decode_iso2(wa) -> <<"Walloon">>;
decode_iso2(wo) -> <<"Wolof">>;
decode_iso2(xh) -> <<"Xhosa">>;
decode_iso2(yi) -> <<"Yiddish (formerly ji)">>;
decode_iso2(yo) -> <<"Yoruba">>;
decode_iso2(za) -> <<"Zhuang">>;
decode_iso2(zh) -> <<"Chinese">>;
decode_iso2(zu) -> <<"Zulu">>;
decode_iso2(I)  -> throw({undefined, I}).


all_iso2() ->
    [{aa, <<"Afar">>},
     {ab, <<"Abkhazian">>},
     {ae, <<"Avestan">>},
     {af, <<"Afrikaans">>},
     {am, <<"Amharic">>},
     {ar, <<"Arabic">>},
     {as, <<"Assamese">>},
     {ay, <<"Aymara">>},
     {az, <<"Azerbaijani">>},
     {ba, <<"Bashkir">>},
     {be, <<"Byelorussian; Belarusian">>},
     {bg, <<"Bulgarian">>},
     {bh, <<"Bihari">>},
     {bi, <<"Bislama">>},
     {bn, <<"Bengali; Bangla">>},
     {bo, <<"Tibetan">>},
     {br, <<"Breton">>},
     {bs, <<"Bosnian">>},
     {ca, <<"Catalan">>},
     {ce, <<"Chechen">>},
     {ch, <<"Chamorro">>},
     {co, <<"Corsican">>},
     {cs, <<"Czech">>},
     {cu, <<"Church Slavic">>},
     {cv, <<"Chuvash">>},
     {cy, <<"Welsh">>},
     {da, <<"Danish">>},
     {de, <<"German">>},
     {dz, <<"Dzongkha; Bhutani">>},
     {el, <<"Greek">>},
     {en, <<"English">>},
     {eo, <<"Esperanto">>},
     {es, <<"Spanish">>},
     {et, <<"Estonian">>},
     {eu, <<"Basque">>},
     {fa, <<"Persian">>},
     {fi, <<"Finnish">>},
     {fj, <<"Fijian; Fiji">>},
     {fo, <<"Faroese">>},
     {fr, <<"French">>},
     {fy, <<"Frisian">>},
     {ga, <<"Irish">>},
     {gd, <<"Scots; Gaelic">>},
     {gl, <<"Gallegan; Galician">>},
     {gn, <<"Guarani">>},
     {gu, <<"Gujarati">>},
     {gv, <<"Manx">>},
     {ha, <<"Hausa (?)">>},
     {he, <<"Hebrew (formerly iw)">>},
     {hi, <<"Hindi">>},
     {ho, <<"Hiri Motu">>},
     {hr, <<"Croatian">>},
     {hu, <<"Hungarian">>},
     {hy, <<"Armenian">>},
     {hz, <<"Herero">>},
     {ia, <<"Interlingua">>},
     {id, <<"Indonesian (formerly in)">>},
     {ie, <<"Interlingue">>},
     {ik, <<"Inupiak">>},
     {io, <<"Ido">>},
     {is, <<"Icelandic">>},
     {it, <<"Italian">>},
     {iu, <<"Inuktitut">>},
     {ja, <<"Japanese">>},
     {jv, <<"Javanese">>},
     {ka, <<"Georgian">>},
     {ki, <<"Kikuyu">>},
     {kj, <<"Kuanyama">>},
     {kk, <<"Kazakh">>},
     {kl, <<"Kalaallisut; Greenlandic">>},
     {km, <<"Khmer; Cambodian">>},
     {kn, <<"Kannada">>},
     {ko, <<"Korean">>},
     {ks, <<"Kashmiri">>},
     {ku, <<"Kurdish">>},
     {kv, <<"Komi">>},
     {kw, <<"Cornish">>},
     {ky, <<"Kirghiz">>},
     {la, <<"Latin">>},
     {lb, <<"Letzeburgesch">>},
     {ln, <<"Lingala">>},
     {lo, <<"Lao; Laotian">>},
     {lt, <<"Lithuanian">>},
     {lv, <<"Latvian; Lettish">>},
     {mg, <<"Malagasy">>},
     {mh, <<"Marshall">>},
     {mi, <<"Maori">>},
     {mk, <<"Macedonian">>},
     {ml, <<"Malayalam">>},
     {mn, <<"Mongolian">>},
     {mo, <<"Moldavian">>},
     {mr, <<"Marathi">>},
     {ms, <<"Malay">>},
     {mt, <<"Maltese">>},
     {my, <<"Burmese">>},
     {na, <<"Nauru">>},
     {nb, <<"Norwegian Bokmål">>},
     {nd, <<"Ndebele, North">>},
     {ne, <<"Nepali">>},
     {ng, <<"Ndonga">>},
     {nl, <<"Dutch">>},
     {nn, <<"Norwegian Nynorsk">>},
     {no, <<"Norwegian">>},
     {nr, <<"Ndebele, South">>},
     {nv, <<"Navajo">>},
     {ny, <<"Chichewa; Nyanja">>},
     {oc, <<"Occitan; Provençal">>},
     {om, <<"(Afan) Oromo">>},
     {'or', <<"Oriya">>},
     {os, <<"Ossetian; Ossetic">>},
     {pa, <<"Panjabi; Punjabi">>},
     {pi, <<"Pali">>},
     {pl, <<"Polish">>},
     {ps, <<"Pashto, Pushto">>},
     {pt, <<"Portuguese">>},
     {qu, <<"Quechua">>},
     {rm, <<"Rhaeto-Romance">>},
     {rn, <<"Rundi; Kirundi">>},
     {ro, <<"Romanian">>},
     {ru, <<"Russian">>},
     {rw, <<"Kinyarwanda">>},
     {sa, <<"Sanskrit">>},
     {sc, <<"Sardinian">>},
     {sd, <<"Sindhi">>},
     {se, <<"Northern Sami">>},
     {sg, <<"Sango; Sangro">>},
     {si, <<"Sinhalese">>},
     {sk, <<"Slovak">>},
     {sl, <<"Slovenian">>},
     {sm, <<"Samoan">>},
     {sn, <<"Shona">>},
     {so, <<"Somali">>},
     {sq, <<"Albanian">>},
     {sr, <<"Serbian">>},
     {ss, <<"Swati; Siswati">>},
     {st, <<"Sesotho; Sotho, Southern">>},
     {su, <<"Sundanese">>},
     {sv, <<"Swedish">>},
     {sw, <<"Swahili">>},
     {ta, <<"Tamil">>},
     {te, <<"Telugu">>},
     {tg, <<"Tajik">>},
     {th, <<"Thai">>},
     {ti, <<"Tigrinya">>},
     {tk, <<"Turkmen">>},
     {tl, <<"Tagalog">>},
     {tn, <<"Tswana; Setswana">>},
     {to, <<"Tonga (?)">>},
     {tr, <<"Turkish">>},
     {ts, <<"Tsonga">>},
     {tt, <<"Tatar">>},
     {tw, <<"Twi">>},
     {ty, <<"Tahitian">>},
     {ug, <<"Uighur">>},
     {uk, <<"Ukrainian">>},
     {ur, <<"Urdu">>},
     {uz, <<"Uzbek">>},
     {vi, <<"Vietnamese">>},
     {vo, <<"Volapuk">>},
     {wa, <<"Walloon">>},
     {wo, <<"Wolof">>},
     {xh, <<"Xhosa">>},
     {yi, <<"Yiddish (formerly ji)">>},
     {yo, <<"Yoruba">>},
     {za, <<"Zhuang">>},
     {zh, <<"Chinese">>},
     {zu, <<"Zulu">>}].

