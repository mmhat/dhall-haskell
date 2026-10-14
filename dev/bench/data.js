window.BENCHMARK_DATA = {
  "lastUpdate": 1791955181635,
  "repoUrl": "https://github.com/mmhat/dhall-haskell",
  "entries": {
    "dhall": [
      {
        "commit": {
          "author": {
            "email": "mmhat@users.noreply.github.com",
            "name": "Mann mit Hut",
            "username": "mmhat"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "45c000f730b846b1e66fe6fa8e95854776641aee",
          "message": "Bumped lower bounds of lsp, lsp-test and lsp-types dependencies (#2873)\n\nThose packages do not have proper upper bounds set on their\ndependencies, which leads to build failures with newer versions of those\ndependencies, e.g. lsp-types-2.1.0.0 and some-1.1 .",
          "timestamp": "2026-10-14T04:57:57Z",
          "tree_id": "1b817fc46b873dcbfc6f725d50d2b1687916b289",
          "url": "https://github.com/mmhat/dhall-haskell/commit/45c000f730b846b1e66fe6fa8e95854776641aee"
        },
        "date": 1791955181155,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "Prelude.issue 412",
            "value": 0.053239426,
            "range": "0.0051273",
            "unit": "ms",
            "extra": "2*Stdev = 0.0051273 ms"
          },
          {
            "name": "Prelude.union performance",
            "value": 0.052846824,
            "range": "0.00491473",
            "unit": "ms",
            "extra": "2*Stdev = 0.00491473 ms"
          },
          {
            "name": "normalize.ChurchEval.typecheck",
            "value": 0.034292947,
            "range": "0.00272756",
            "unit": "ms",
            "extra": "2*Stdev = 0.00272756 ms"
          },
          {
            "name": "normalize.ChurchEval.evaluation",
            "value": 400.732108,
            "range": "9.10924",
            "unit": "ms",
            "extra": "2*Stdev = 9.10924 ms"
          },
          {
            "name": "normalize.FunCompose.typecheck",
            "value": 0.018027299,
            "range": "0.00135458",
            "unit": "ms",
            "extra": "2*Stdev = 0.00135458 ms"
          },
          {
            "name": "normalize.FunCompose.evaluation",
            "value": 305.355575,
            "range": "6.14935",
            "unit": "ms",
            "extra": "2*Stdev = 6.14935 ms"
          },
          {
            "name": "normalize.Iterate.typecheck",
            "value": 0.046520699,
            "range": "0.00281655",
            "unit": "ms",
            "extra": "2*Stdev = 0.00281655 ms"
          },
          {
            "name": "normalize.Iterate.evaluation",
            "value": 86.3055056,
            "range": "3.82251",
            "unit": "ms",
            "extra": "2*Stdev = 3.82251 ms"
          },
          {
            "name": "normalize.IterateAlt.typecheck",
            "value": 0.020371126,
            "range": "0.00161131",
            "unit": "ms",
            "extra": "2*Stdev = 0.00161131 ms"
          },
          {
            "name": "normalize.IterateAlt.evaluation",
            "value": 264.8693846,
            "range": "2.7224",
            "unit": "ms",
            "extra": "2*Stdev = 2.7224 ms"
          },
          {
            "name": "normalize.IterateAlt2.typecheck",
            "value": 0.018366322,
            "range": "0.0013903",
            "unit": "ms",
            "extra": "2*Stdev = 0.0013903 ms"
          },
          {
            "name": "normalize.IterateAlt2.evaluation",
            "value": 208.4966082,
            "range": "2.86087",
            "unit": "ms",
            "extra": "2*Stdev = 2.86087 ms"
          },
          {
            "name": "normalize.ListBench.typecheck",
            "value": 0.053834022,
            "range": "0.00405902",
            "unit": "ms",
            "extra": "2*Stdev = 0.00405902 ms"
          },
          {
            "name": "normalize.ListBench.evaluation",
            "value": 186.8341882,
            "range": "4.61912",
            "unit": "ms",
            "extra": "2*Stdev = 4.61912 ms"
          },
          {
            "name": "normalize.ListBenchAlt.typecheck",
            "value": 0.049255875,
            "range": "0.00328202",
            "unit": "ms",
            "extra": "2*Stdev = 0.00328202 ms"
          },
          {
            "name": "normalize.ListBenchAlt.evaluation",
            "value": 234.4467942,
            "range": "5.2672",
            "unit": "ms",
            "extra": "2*Stdev = 5.2672 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.typecheck",
            "value": 0.000490906,
            "range": "4.2176e-05",
            "unit": "ms",
            "extra": "2*Stdev = 4.2176e-05 ms"
          },
          {
            "name": "normalize.NaturalFoldShortcut.evaluation",
            "value": 0.010898331,
            "range": "0.000667582",
            "unit": "ms",
            "extra": "2*Stdev = 0.000667582 ms"
          },
          {
            "name": "large1.parse",
            "value": 174.7852752,
            "range": "3.68408",
            "unit": "ms",
            "extra": "2*Stdev = 3.68408 ms"
          },
          {
            "name": "large1.resolve",
            "value": 61.5582923,
            "range": "2.71038",
            "unit": "ms",
            "extra": "2*Stdev = 2.71038 ms"
          },
          {
            "name": "large1.typecheck",
            "value": 73.0360762,
            "range": "3.09736",
            "unit": "ms",
            "extra": "2*Stdev = 3.09736 ms"
          },
          {
            "name": "large1.evaluation",
            "value": 142.0345816,
            "range": "4.42575",
            "unit": "ms",
            "extra": "2*Stdev = 4.42575 ms"
          },
          {
            "name": "large2.normalize",
            "value": 166.2909694,
            "range": "3.00181",
            "unit": "ms",
            "extra": "2*Stdev = 3.00181 ms"
          },
          {
            "name": "large2.cbor.encode",
            "value": 336.699408,
            "range": "6.4756",
            "unit": "ms",
            "extra": "2*Stdev = 6.4756 ms"
          },
          {
            "name": "large2.cbor.decode",
            "value": 835.4820868,
            "range": "6.32676",
            "unit": "ms",
            "extra": "2*Stdev = 6.32676 ms"
          },
          {
            "name": "k8s.file3.resolve",
            "value": 2481.7824814,
            "range": "91.3468",
            "unit": "ms",
            "extra": "2*Stdev = 91.3468 ms"
          },
          {
            "name": "k8s.file3.typecheck",
            "value": 404.5664882,
            "range": "17.0696",
            "unit": "ms",
            "extra": "2*Stdev = 17.0696 ms"
          },
          {
            "name": "k8s.file3.evaluation",
            "value": 0.675430609,
            "range": "0.0250614",
            "unit": "ms",
            "extra": "2*Stdev = 0.0250614 ms"
          },
          {
            "name": "k8s.file4.resolve",
            "value": 1578.7024246,
            "range": "16.1019",
            "unit": "ms",
            "extra": "2*Stdev = 16.1019 ms"
          },
          {
            "name": "k8s.file4.typecheck",
            "value": 93.2775066,
            "range": "2.78559",
            "unit": "ms",
            "extra": "2*Stdev = 2.78559 ms"
          },
          {
            "name": "k8s.file4.evaluation",
            "value": 0.412445829,
            "range": "0.0227306",
            "unit": "ms",
            "extra": "2*Stdev = 0.0227306 ms"
          },
          {
            "name": "large3.resolve",
            "value": 2531.55590975,
            "range": "150.38",
            "unit": "ms",
            "extra": "2*Stdev = 150.38 ms"
          },
          {
            "name": "large3.typecheck",
            "value": 4976.0875876,
            "range": "61.7488",
            "unit": "ms",
            "extra": "2*Stdev = 61.7488 ms"
          },
          {
            "name": "large3.evaluation",
            "value": 721.5158718,
            "range": "16.999",
            "unit": "ms",
            "extra": "2*Stdev = 16.999 ms"
          },
          {
            "name": "large3.get_config.resolve",
            "value": 2530.9281658,
            "range": "161.529",
            "unit": "ms",
            "extra": "2*Stdev = 161.529 ms"
          },
          {
            "name": "large3.get_config.typecheck",
            "value": 5135.0145772,
            "range": "21.0488",
            "unit": "ms",
            "extra": "2*Stdev = 21.0488 ms"
          },
          {
            "name": "large3.get_config.evaluation",
            "value": 0.262192631,
            "range": "0.0145351",
            "unit": "ms",
            "extra": "2*Stdev = 0.0145351 ms"
          },
          {
            "name": "large4.resolve",
            "value": 320.3656084,
            "range": "8.47291",
            "unit": "ms",
            "extra": "2*Stdev = 8.47291 ms"
          },
          {
            "name": "large4.typecheck",
            "value": 425.9359994,
            "range": "9.79794",
            "unit": "ms",
            "extra": "2*Stdev = 9.79794 ms"
          },
          {
            "name": "large4.evaluation",
            "value": 1.898514768,
            "range": "0.144639",
            "unit": "ms",
            "extra": "2*Stdev = 0.144639 ms"
          },
          {
            "name": "large5.resolve",
            "value": 137.0557274,
            "range": "4.62232",
            "unit": "ms",
            "extra": "2*Stdev = 4.62232 ms"
          },
          {
            "name": "large5.typecheck",
            "value": 67.4792094,
            "range": "2.06142",
            "unit": "ms",
            "extra": "2*Stdev = 2.06142 ms"
          },
          {
            "name": "large5.evaluation",
            "value": 73.6828338,
            "range": "5.23512",
            "unit": "ms",
            "extra": "2*Stdev = 5.23512 ms"
          },
          {
            "name": "large6.slow_parse.resolve",
            "value": 822.5335784,
            "range": "10.0283",
            "unit": "ms",
            "extra": "2*Stdev = 10.0283 ms"
          },
          {
            "name": "large6.slow_parse.typecheck",
            "value": 0.000207982,
            "range": "1.4006e-05",
            "unit": "ms",
            "extra": "2*Stdev = 1.4006e-05 ms"
          },
          {
            "name": "large6.slow_parse.evaluation",
            "value": 0.000216656,
            "range": "6.698e-06",
            "unit": "ms",
            "extra": "2*Stdev = 6.698e-06 ms"
          },
          {
            "name": "large6.slow_walk.resolve",
            "value": 434.7735164,
            "range": "4.04844",
            "unit": "ms",
            "extra": "2*Stdev = 4.04844 ms"
          },
          {
            "name": "large6.slow_walk.typecheck",
            "value": 1.170603578,
            "range": "0.0467583",
            "unit": "ms",
            "extra": "2*Stdev = 0.0467583 ms"
          },
          {
            "name": "large6.slow_walk.evaluation",
            "value": 2.264390006,
            "range": "0.175489",
            "unit": "ms",
            "extra": "2*Stdev = 0.175489 ms"
          },
          {
            "name": "large6.slow_eval.resolve_cold_cache_on",
            "value": 1.431566731,
            "range": "0.124599",
            "unit": "ms",
            "extra": "2*Stdev = 0.124599 ms"
          },
          {
            "name": "large6.slow_typecheck.resolve_cold_cache_on",
            "value": 528.2932028,
            "range": "5.23558",
            "unit": "ms",
            "extra": "2*Stdev = 5.23558 ms"
          },
          {
            "name": "large6.slow_normalize.resolve_cold_cache_on",
            "value": 480.0552738,
            "range": "13.9512",
            "unit": "ms",
            "extra": "2*Stdev = 13.9512 ms"
          },
          {
            "name": "large6.slow_multi.resolve_cold_cache_on",
            "value": 387.0474852,
            "range": "11.3704",
            "unit": "ms",
            "extra": "2*Stdev = 11.3704 ms"
          },
          {
            "name": "prelude_import.resolve_cold_cache_on",
            "value": 353.2160718,
            "range": "7.63313",
            "unit": "ms",
            "extra": "2*Stdev = 7.63313 ms"
          },
          {
            "name": "diamond_import.end_to_end_cold",
            "value": 487.7963686,
            "range": "8.50274",
            "unit": "ms",
            "extra": "2*Stdev = 8.50274 ms"
          },
          {
            "name": "diamond_import_transitive.end_to_end_cold",
            "value": 480.7330166,
            "range": "43.617",
            "unit": "ms",
            "extra": "2*Stdev = 43.617 ms"
          },
          {
            "name": "substitutions.resolve_cold_cache_on",
            "value": 191.5354474,
            "range": "5.472",
            "unit": "ms",
            "extra": "2*Stdev = 5.472 ms"
          },
          {
            "name": "substitutions.many_files.resolve_cold_cache_on",
            "value": 83.0788749,
            "range": "4.99082",
            "unit": "ms",
            "extra": "2*Stdev = 4.99082 ms"
          },
          {
            "name": "substitutions.composer_proxy.end_to_end_cold",
            "value": 941.844247,
            "range": "9.20647",
            "unit": "ms",
            "extra": "2*Stdev = 9.20647 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.cold",
            "value": 383.4636398,
            "range": "4.75874",
            "unit": "ms",
            "extra": "2*Stdev = 4.75874 ms"
          },
          {
            "name": "substitutions.composer_proxy.many_imports.warm",
            "value": 188.9380784,
            "range": "5.3336",
            "unit": "ms",
            "extra": "2*Stdev = 5.3336 ms"
          },
          {
            "name": "substitutions.shift_cost.naive",
            "value": 22.8384103,
            "range": "1.46548",
            "unit": "ms",
            "extra": "2*Stdev = 1.46548 ms"
          },
          {
            "name": "substitutions.shift_cost.optimized",
            "value": 1.811378931,
            "range": "0.08658",
            "unit": "ms",
            "extra": "2*Stdev = 0.08658 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.full",
            "value": 47.6081624,
            "range": "3.71921",
            "unit": "ms",
            "extra": "2*Stdev = 3.71921 ms"
          },
          {
            "name": "semisemantic.nf_size_walk.early_abort",
            "value": 1.403116893,
            "range": "0.02337",
            "unit": "ms",
            "extra": "2*Stdev = 0.02337 ms"
          },
          {
            "name": "Issue #108.Text",
            "value": 6.909487562,
            "range": "0.359668",
            "unit": "ms",
            "extra": "2*Stdev = 0.359668 ms"
          },
          {
            "name": "Issue #108.Binary",
            "value": 0.14237018,
            "range": "0.0113273",
            "unit": "ms",
            "extra": "2*Stdev = 0.0113273 ms"
          },
          {
            "name": "Kubernetes/Binary",
            "value": 295.2054227,
            "range": "28.3301",
            "unit": "ms",
            "extra": "2*Stdev = 28.3301 ms"
          },
          {
            "name": "Deeply nested parentheses",
            "value": 14.424109,
            "range": "0.370942",
            "unit": "ms",
            "extra": "2*Stdev = 0.370942 ms"
          },
          {
            "name": "Deeply nested brackets",
            "value": 14.0372575,
            "range": "1.22449",
            "unit": "ms",
            "extra": "2*Stdev = 1.22449 ms"
          },
          {
            "name": "Long variable names",
            "value": 30.98111,
            "range": "1.43625",
            "unit": "ms",
            "extra": "2*Stdev = 1.43625 ms"
          },
          {
            "name": "Large number of function arguments",
            "value": 41.8696936,
            "range": "2.50039",
            "unit": "ms",
            "extra": "2*Stdev = 2.50039 ms"
          },
          {
            "name": "Long double-quoted strings (10M chars)",
            "value": 218.3882888,
            "range": "5.80887",
            "unit": "ms",
            "extra": "2*Stdev = 5.80887 ms"
          },
          {
            "name": "Long single-quoted strings (10M chars)",
            "value": 211.3414506,
            "range": "7.64888",
            "unit": "ms",
            "extra": "2*Stdev = 7.64888 ms"
          },
          {
            "name": "Large natural number literal (10M digits)",
            "value": 195.6800242,
            "range": "9.46847",
            "unit": "ms",
            "extra": "2*Stdev = 9.46847 ms"
          },
          {
            "name": "Large hex number literal (10M digits)",
            "value": 217.7675309,
            "range": "4.61404",
            "unit": "ms",
            "extra": "2*Stdev = 4.61404 ms"
          },
          {
            "name": "Large binary number literal (10M digits)",
            "value": 206.7837202,
            "range": "9.66204",
            "unit": "ms",
            "extra": "2*Stdev = 9.66204 ms"
          },
          {
            "name": "Large natural number literal (10M digits, forced)",
            "value": 1625.0967806,
            "range": "35.5858",
            "unit": "ms",
            "extra": "2*Stdev = 35.5858 ms"
          },
          {
            "name": "Large hex number literal (10M digits, forced)",
            "value": 377.513547,
            "range": "9.08195",
            "unit": "ms",
            "extra": "2*Stdev = 9.08195 ms"
          },
          {
            "name": "Large binary number literal (10M digits, forced)",
            "value": 292.9532932,
            "range": "19.718",
            "unit": "ms",
            "extra": "2*Stdev = 19.718 ms"
          },
          {
            "name": "Whitespace",
            "value": 23.1512472,
            "range": "1.89688",
            "unit": "ms",
            "extra": "2*Stdev = 1.89688 ms"
          },
          {
            "name": "Line comment",
            "value": 452.7774132,
            "range": "10.9441",
            "unit": "ms",
            "extra": "2*Stdev = 10.9441 ms"
          },
          {
            "name": "Block comment",
            "value": 313.2553468,
            "range": "6.64134",
            "unit": "ms",
            "extra": "2*Stdev = 6.64134 ms"
          },
          {
            "name": "CPkg.parse",
            "value": 1292.736045,
            "range": "34.4482",
            "unit": "ms",
            "extra": "2*Stdev = 34.4482 ms"
          },
          {
            "name": "CPkg.Text",
            "value": 1341.127627,
            "range": "75.511",
            "unit": "ms",
            "extra": "2*Stdev = 75.511 ms"
          }
        ]
      }
    ]
  }
}