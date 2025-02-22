# Overview

`ada_caser` adjusts the casing of Ada source files to match the standard typified in the Ada Reference Manual.

These tests expect to be run in the root directory of `ada_caser`, so that the executable is in `bin/`.

## Feature 1: Simple usage

### Scenario 1.1: Doesn't touch already-formatted files

- Given the new file `simple.ads`
```
package Simple is
end Simple;
```
- When I run `bin/ada_caser simple.ads`
- then the output is
```
package Simple is
end Simple;
```

### Scenario 1.2: Keywords are lowcased, identifiers are title-cased

- Given the new file `keywords_identifiers.ads`
```
Package keywords_identifiers Is
END Keywords_Identifiers;
```
- when I run `bin/ada_caser keywords_identifiers.ads`
- then the output is
```
package Keywords_Identifiers is
end Keywords_Identifiers;
```

### Scenario 1.3: Characters, strings and numbers aren't touched

- Given the new file `c_s_n.ads`
```
package C_S_N Is

   C_L : Character := 'c';
   C_U : Character := 'C';

   S_U : String := "UPPER";
   S_L : String := "lower";
   S_M : String := "mIxEd";

   N_U : Integer := 16#ABCDEF#;
   N_L : Integer := 16#abcdef#;
   N_M : Integer := 16#aBcDeF#;

end C_S_N;
```
- when I run `bin/ada_caser c_s_n.ads`
- then the output is
```
package C_S_N is

   C_L : Character := 'c';
   C_U : Character := 'C';

   S_U : String := "UPPER";
   S_L : String := "lower";
   S_M : String := "mIxEd";

   N_U : Integer := 16#ABCDEF#;
   N_L : Integer := 16#abcdef#;
   N_M : Integer := 16#aBcDeF#;

end C_S_N;
```

### Scenario 1.4: Only the first character of an identifier is touched

- Given the new file `first_char.ads` containing `package aBcD is end abCd;`
- when I run `bin/ada_caser first_char.ads`
- then the output is `package ABcD is end AbCd;`

### Scenario 1.5: International characters

- Given the new file `internationals.ads` containing
```
package internationals is
   type enum is
     (item,
      'a',
      '­',  -- this is a soft hyphen
      ae_ææ_ae,
      '-',
      'ÿ');
end internationals;
```
- when I run `bin/ada_caser internationals.ads`
- then the output is
```
package Internationals is
   type Enum is
     (Item,
      'a',
      '­',  -- this is a soft hyphen
      Ae_Ææ_Ae,
      '-',
      'ÿ');
end Internationals;
```

## Feature 2: Case exceptions

### Scenario 2.1: Whole words

- Given the new file `whole_words.ads`
```
package whole_words is
end WHOLE_WORDS;
```

- and given the new file `whole_words.dict` containing `Whole_WoRdS`
- when I run `bin/ada_caser --dictionary whole_words.dict whole_words.ads`
- then the output is
```
package Whole_WoRdS is
end Whole_WoRdS;
```

### Scenario 2.2: Part words

- Given the new file `part-words.ads`
```
package part.words is
  SOME_words : integer;
end PART.WORDS;
```

- and given the new file `part_words.dict` containing `*WoRdS`
- when I run `bin/ada_caser --dictionary part_words.dict part-words.ads`
- then the output is
```
package Part.WoRdS is
  SOME_WoRdS : Integer;
end PART.WoRdS;
```

### Scenario 2.3: Commented and blank lines are ignored

- Given the new file `commented.dict` containing
```
# This is a comment

ESP32
```
- when I run `bin/ada_caser --dictionary=commented.dict`
- then there is no output

### Scenario 2.4: An exception can be indented

- Given the new file `indented.dict` containing
```
   # This is a comment

   FOO
```
- and given the new file `foo.ads` containing `procedure foo;`
- when I run `bin/ada_caser -D indented.dict foo.ads`
- then the output is `procedure FOO;`

### Scenario 2.5: Duplicate but identical exceptions are accepted

- Given the new file `duplicates.dict` containing
```
ESP32
*SPI
ESP32
*SPI
```
- when I run `bin/ada_caser --dictionary duplicates.dict`
- then there is no output

### Scenario 2.6: Exceptions that are only different by case are rejected

- Given the new file `differing-in-case.dict` containing
```
ESP32
*STM
ESp32
*sTM
```
- when I run `bin/ada_caser -D differing-in-case.dict`
- then the output is
```
Error: Case exception (ESP32) already found for 'ESp32'
Error: Sub-case exception (*STM) already found for '*sTM'
```

### Scenario 2.7: International characters are supported in casing exceptions

- Given the new file `international-exceptions.dict` containing 
```
ÀḆĈḒË
```
- and the new file `international_identifier.ads` containing
```
package international_identifier is
   àḇĉḓë : constant := 42;
end international_identifier;
```
- when I run `bin/ada_caser --dictionary international-exceptions.dict international_identifier.ads`
- then the output is
```
package International_Identifier is
   ÀḆĈḒË : constant := 42;
end International_Identifier;
```
