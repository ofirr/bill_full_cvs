/* $Header: /baz/users/cvs-root/Source/system/transform/nil.cp,v 1.1.1.1 1993/12/31 10:42:39 fcp Exp $ */
-export([transform/5]).
-mode(trust).

transform(Attributes1, Terms1, Attributes2, Terms2, Errs) :-

    true : Terms1 = _,
      Attributes2 = Attributes1,
      Terms2 = [boot(`"_", `"_")],
      Errs = [] .
