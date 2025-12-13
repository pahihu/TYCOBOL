#
# History:
# 251213AP  moved WSSCRN to the beginning of DATA DIVISION
#           group output fields into SC-OUTPUT-FIELDS
# 251211AP  screen options for fields also
# 251201AP  MEMBER var
# 251129AP  remove ':' from field names, 'o' marks field read-only
#
BEGIN \
{
  # MEMBER is the name
  MAXSP = 1;
  fgcol = ENVIRON["COB_FG_COLOR"];
  bgcol = ENVIRON["COB_BG_COLOR"];
  if (0 == +fgcol) fgcol = 7;
  if (0 == +bgcol) bgcol = 1;
  PRG = toupper(MEMBER);
  n = index(PRG, "/"); if (n) PRG = substr(PRG, n+1);

  # fields[x] = fld^fmt
  # outfields[x] = fld^fmt
  nfields = 0; noutfields = 0;
  # scrlines[x] = line
  nscrlines = 0;

  # scropt[]
  # r   required
  # fN  fg color
  # bN  bg color
  # o   read-only field, FROM
  # h   highlight
  # n   normal video
}

function trim(s ,n)
{
  while (substr(s,length(s),1) == " ") s = substr(s,1,length(s)-1);
  return s;
}

function quote(s)
{
    gsub("'","''",s);
    return s;
}

function mkfield(s)
{
    # printf "mkfield(\"%s\")\n",s;
    gsub(" ","-",s);
    # remove :
    gsub(":","",s);
    gsub("\\.","",s);
    gsub("/","-",s);
    return "SC-" toupper(s);
}

function appfmt(s,pfmt,cnt)
{
  if (cnt) {
    if (cnt > 1) {
      s = s sprintf("%c(%d)",pfmt,cnt); 
    } else s = s pfmt;
  }
  return s;
}

function zipfmt(s ,c,i,ret,cnt,pfmt)
{
  ret = ""; cnt = 0; pfmt = "";
  for (i=1;i<=length(s);i++) {
    c = substr(s,i,1);
    if (cnt) {
      if (c == pfmt) {
        cnt++;
        continue;
      } else {
        ret = appfmt(ret,pfmt,cnt); cnt = 0;
      }
    }
    if ("X" == c) {
      pfmt = c; cnt = 1;
      continue;
    }
    if ("9" == c) {
      pfmt = c; cnt = 1;
      continue;
    }
    ret = appfmt(ret,pfmt,cnt); cnt = 0;
    ret = ret c;
  }
  ret = appfmt(ret,pfmt,cnt);
  return ret;
}

function scr2dat(s)
{
  gsub("Z","9",s);
  gsub(",","",s);
  gsub("/","",s);
  gsub("\\.","V",s);
  return zipfmt(s);
}

function getscropt(s ,c)
{
  delete scropt;
  scropt["any"] = 0;
  c = substr(s,1,1);
  while ("a" <= c && c <= "z") {
    scropt["any"] = 1;
    s = substr(s,2);
    if ("n" == c) scropt[c] = 1;
    if ("o" == c) scropt[c] = 1;
    if ("r" == c) scropt[c] = 1;
    if ("h" == c) scropt[c] = 1;
    if ("b" == c) { scropt[c] = substr(s,1,1); s = substr(s,2); }
    if ("f" == c) { scropt[c] = substr(s,1,1); s = substr(s,2); }
    c = substr(s,1,1);
  }
  return s;
}

function addopts()
{
    if ("h" in scropt)
        scrlines[nscrlines++] = sprintf("%31sHIGHLIGHT\n","");
    if ("f" in scropt)
        scrlines[nscrlines++] = sprintf("%31sFOREGROUND-COLOR %d\n","",scropt["f"]);
    if ("b" in scropt)
        scrlines[nscrlines++] = sprintf("%31sBACKGROUND-COLOR %d\n","",scropt["b"]);
}

function emit(trig, s ,fld,fmt)
{
  s = trim(s);
  # printf "emit: trig=%s s='%s'\n",trig,s;
  if (length(s)) {
    if ("[" == substr(s,1,1)) {
        if (length(prev_fld)) {
            fld = prev_fld;
            prev_fld = "";
        }
        else
            fld = sprintf("WS-%d", NR);
        fmt = getscropt(substr(s,2,length(s)-2));
        scrlines[nscrlines++] = sprintf("%10s03 LINE %02d COLUMN %02d PIC %s\n", "", NR, col, zipfmt(fmt), fld);
        if (0 == ("n" in scropt))
            scrlines[nscrlines++] = sprintf("%31sREVERSE-VIDEO\n","");
        addopts();
        if ("r" in scropt)
            scrlines[nscrlines++] = sprintf("%31sREQUIRED\n","");
        using = "USING";
        if ("o" in scropt) {
            using ="FROM";
            outfields[noutfields++] = fld "^" fmt;
        } else {
            fields[nfields++] = fld "^" fmt;
        }
        scrlines[nscrlines++] = sprintf("%31s%s %s.\n","",using,fld);
    } else {
        s = quote(getscropt(s));
        if (scropt["any"]) dot = "";
        else dot = ".";
        scrlines[nscrlines++] = sprintf("%10s03 LINE %02d COLUMN %02d VALUE '%s'%s\n", "", NR, col, s, dot);
        addopts();
        if (dot == "") {
            tmp = scrlines[nscrlines-1];
            scrlines[nscrlines-1] = substr(tmp,1,length(tmp)-1) ".\n";
        }
        prev_fld = mkfield(s);
        # printf "prev_fld='%s'\n",prev_fld;
    }
  }
  col = 0;
}

{
  if (0 ==NF) next;
  lit = ""; col = 0; prev_fld = "";
  for (i = 1; i <= length($0); i++) {
    c = substr($0,i,1);
    if (" " == c) {
      if (col) {
        spcnt++;
        if (spcnt > MAXSP) {
          if (col) {
            emit("MAXSP",lit);
            continue;
          }
        }
      } else continue;
    }
    if ("[" == c) {
      if (col) emit("[", lit);
    }
    # printf "LINE %d COL %d\n", NR, i;
    if (0 == col) {
      col = i;
      lit = "";
      spcnt = 0;
    }
    if (col) {
      lit = lit c;
      spcnt = 0;
      # printf "lit='%s'\n", lit;
    }
    if ("]" == c) {
        emit("]",lit);
    }
  }
  if (col) emit("EOL",lit);
}

END \
{
    printf "       IDENTIFICATION DIVISION.\n";
    printf "       PROGRAM-ID. %s.\n", PRG;
    printf "       AUTHOR. John Doe.\n";
    printf "       COPY SCRNIO.\n";
    printf "      *-----------------------------------------------------\n";
    printf "       DATA DIVISION.\n";
    printf "       WORKING-STORAGE SECTION.\n";
    printf "       COPY WSSCRN.\n";
    printf "      *---------------------BEGIN-PAN2SCR-------------------\n";
    for (x = 0; x < nfields; x++) {
        n = split(fields[x],parts,"^");
        fld = parts[1]; fmt = parts[2];
        fmt = scr2dat(fmt);
        if ("X" == substr(fmt,1,1)) blank = "SPACES";
        else blank = "ZEROS";
        printf "       01 %-20s PIC %s VALUE %s.\n", fld, fmt, blank;
    }
    if (noutfields)
        printf "       01 SC-OUTPUT-FIELDS.\n";
    for (x = 0; x < noutfields; x++) {
        n = split(outfields[x],parts,"^");
        fld = parts[1]; fmt = parts[2];
        fmt = scr2dat(fmt);
        if ("X" == substr(fmt,1,1)) blank = "SPACES";
        else blank = "ZEROS";
        printf "           03 %-20s PIC %s VALUE %s.\n", fld, fmt, blank;
    }
    printf "      *-----------------------------------------------------\n";
    printf "       SCREEN SECTION.\n"; printf "       01 %s-SCREEN\n", PRG;
    printf "          BLANK SCREEN, AUTO,\n";
    printf "          FOREGROUND-COLOR IS %d,\n", fgcol;
    printf "          BACKGROUND-COLOR IS %d.\n", bgcol;
    for (x = 0; x < nscrlines; x++) {
        printf "%s", scrlines[x];
    }

    printf "      *----------------------END-PAN2SCR--------------------\n";
    printf "       PROCEDURE DIVISION.\n";
    printf "           DISPLAY %s-SCREEN\n", PRG;
    printf "           ACCEPT %s-SCREEN\n", PRG;
    printf "           STOP RUN\n";
    printf "           .\n";
}

# vim:set ts=4 sw=4 et:
