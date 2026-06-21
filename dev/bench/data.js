window.BENCHMARK_DATA = {
  "lastUpdate": 1782066996299,
  "repoUrl": "https://github.com/jdh8/dds-bridge",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "committer": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "distinct": true,
          "id": "de4c62d3646ebcf5e7c0ca7105a6dcf473c78e3c",
          "message": "Bump dds-bridge-sys to 3.2 and drop path-patch override\n\nThe batched FFI entries (dds_calc_dd_tables_batched,\ndds_solve_boards_batched) and the DDS 3.0.0 vendor refresh that\nsolve_deals / solve_boards / system_info_version_is_3_0_0 already\ndepend on are now in a published dds-bridge-sys release. Switch the\nCargo.toml requirement to \"3.2\", remove the [patch.crates-io] override\nthat was sourcing them locally, and resolve the Unreleased section's\n\"dds-bridge-sys Unreleased\" references to \"3.2.0\". Also pick up a\nstray cargo fmt cleanup in src/tricks.rs.\n\nCo-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>",
          "timestamp": "2026-05-25T05:20:59+08:00",
          "tree_id": "e010953b85193b96ceffce56802c44b01878bf59",
          "url": "https://github.com/jdh8/dds-bridge/commit/de4c62d3646ebcf5e7c0ca7105a6dcf473c78e3c"
        },
        "date": 1779658339662,
        "tool": "cargo",
        "benches": [
          {
            "name": "solve_deal_single",
            "value": 244529488,
            "range": "± 570034059",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/32",
            "value": 5280377318,
            "range": "± 38970943",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/200",
            "value": 33075624469,
            "range": "± 116616171",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/32",
            "value": 385017403,
            "range": "± 969091",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/200",
            "value": 2268490063,
            "range": "± 6837423",
            "unit": "ns/iter"
          },
          {
            "name": "analyse_plays_32",
            "value": 247067938,
            "range": "± 12919694",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "committer": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "distinct": true,
          "id": "43a1ddb785d98dd2c3859979c4074ce296121770",
          "message": "CI: pin macOS runner to macos-26\n\nMirrors dds-bridge-sys dc291d3. `macos-latest` currently resolves to\nmacos-15 (Sequoia, SDK 15.5), whose libc++ gates `std::jthread` behind\nan availability marker the deployment target does not satisfy. The\nDDS vendor refresh that landed in dds-bridge-sys 3.2.0 pulled in\nupstream eb3290b (\"parallelize solve_all_boards_n with std::jthread\npool\"), so vendor/library/src/solve_board.cpp now instantiates\n`std::vector<std::jthread>` and fails to compile on macos-15. Since\ndds-bridge builds dds-bridge-sys from source via cc, the test matrix\nhits the same break. Match upstream dds-bridge/dds, which moved its\nown macOS CI to macos-26 (Tahoe, Xcode 26) in commits 9ef7bb9 +\n8d4b1dc.\n\nCo-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>",
          "timestamp": "2026-05-25T05:25:53+08:00",
          "tree_id": "0be33ec2e926cf7ca04f23927946518c30f58927",
          "url": "https://github.com/jdh8/dds-bridge/commit/43a1ddb785d98dd2c3859979c4074ce296121770"
        },
        "date": 1779658635686,
        "tool": "cargo",
        "benches": [
          {
            "name": "solve_deal_single",
            "value": 250003583,
            "range": "± 585792155",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/32",
            "value": 5354376600,
            "range": "± 35524869",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/200",
            "value": 33662468973,
            "range": "± 103352196",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/32",
            "value": 387441393,
            "range": "± 1759144",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/200",
            "value": 2287258885,
            "range": "± 11206314",
            "unit": "ns/iter"
          },
          {
            "name": "analyse_plays_32",
            "value": 251148004,
            "range": "± 11821406",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "committer": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "distinct": true,
          "id": "fdf17b25b311924e8b3ab6d8837b39884482d720",
          "message": "Lock dds-bridge-sys to 3.2.1 and note the perf / crash-fix in CHANGELOG\n\ndds-bridge-sys 3.2.1 ships a persistent FFI WorkerPool (one long-lived\nSolverContext per worker) and an ab_search inlining +\nshared_ptr<ThreadData>→raw-pointer cleanup in the vendored DDS. The\nCargo.toml requirement was already \"3.2\" so no manifest change is\nneeded; this commit just amends the [Unreleased] entry in CHANGELOG.md\nto record what 3.2.1 brings and the measured impact.\n\nReproduced 3.2.0's SIGSEGV in TransTableL::lookup_suit /\nMoves::MergeSort on this 7950X3D (32 threads, Linux x86_64) — cargo\nbench --bench solver crashed mid-solve_deals/32. Re-ran clean on\n3.2.1; batched-bench timings (solve_deals/200 3669 ms, solve_boards/200\n258 ms) sit within ~2% of the upstream-changelog \"after\" column, so\nthe reported -11% to -14% gain applies here too.\ncargo test --all-features (32 tests, incl.\nsolve_deals_large_batch_matches_sequential) passes.\n\nCo-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>",
          "timestamp": "2026-05-26T03:27:15+08:00",
          "tree_id": "bfb03538913581d7bd70bb1a930f4f442e944b05",
          "url": "https://github.com/jdh8/dds-bridge/commit/fdf17b25b311924e8b3ab6d8837b39884482d720"
        },
        "date": 1779737847677,
        "tool": "cargo",
        "benches": [
          {
            "name": "solve_deal_single",
            "value": 235689916,
            "range": "± 568219118",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/32",
            "value": 4677137543,
            "range": "± 31317166",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/200",
            "value": 29245526618,
            "range": "± 181275513",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/32",
            "value": 340991460,
            "range": "± 2707197",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/200",
            "value": 1987515950,
            "range": "± 6159524",
            "unit": "ns/iter"
          },
          {
            "name": "analyse_plays_32",
            "value": 216873816,
            "range": "± 12346982",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "committer": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "distinct": true,
          "id": "844294b51aa5b4ca3e0e660fb014ad8fe1818588",
          "message": "ci(readme): split benchmark workflow and update badges",
          "timestamp": "2026-06-01T07:26:28+08:00",
          "tree_id": "c5797136d88e511f5145e05140f0b6ae17649bfb",
          "url": "https://github.com/jdh8/dds-bridge/commit/844294b51aa5b4ca3e0e660fb014ad8fe1818588"
        },
        "date": 1780270644187,
        "tool": "cargo",
        "benches": [
          {
            "name": "solve_deal_single",
            "value": 217659440,
            "range": "± 524425966",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/32",
            "value": 5198416565,
            "range": "± 36828981",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/200",
            "value": 32959139875,
            "range": "± 185434404",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/32",
            "value": 353797982,
            "range": "± 2023614",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/200",
            "value": 2235191936,
            "range": "± 5287708",
            "unit": "ns/iter"
          },
          {
            "name": "analyse_plays_32",
            "value": 254767654,
            "range": "± 10629167",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "committer": {
            "email": "chen.pang.he@jdh8.org",
            "name": "Chen-Pang He",
            "username": "jdh8"
          },
          "distinct": true,
          "id": "3bda7b69696d53fd85dd9fe8616edf9dfde449fc",
          "message": "Bump dds-bridge-sys to 3.3.0 (upstream develop refresh, fork retired)\n\n3.3.0 retires the jdh8/dds fork and refreshes the vendored DDS to upstream\ndds-bridge/dds develop (v3.0.0-240-g0700b42). The two fork-only patches\nbehind the 3.2.1 perf/crash-fix are now upstream (PR #191's thread_ptr()\nfor the shared_ptr churn; scheduler.RegisterRun dropped from\ncalc_tables.cpp), so the gain and the batched-concurrency crash-fix carry\nover with no fork code.\n\nThe public DDS ABI (api/dll.h) is unchanged, so dds-bridge needs no source\nchanges; tighten the requirement to \"3.3\". Until 3.3.0 is on crates.io, a\n[patch.crates-io] override sources it from the dds-bridge-sys GitHub repo\n(CI does a clean checkout, so a path override can't work). cargo test\n--all-features passes, including the ddss-parity batch tests.\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>",
          "timestamp": "2026-06-22T02:17:31+08:00",
          "tree_id": "ceb89e89e89b855aa8f1a3f3da957e0593c17bb2",
          "url": "https://github.com/jdh8/dds-bridge/commit/3bda7b69696d53fd85dd9fe8616edf9dfde449fc"
        },
        "date": 1782066996023,
        "tool": "cargo",
        "benches": [
          {
            "name": "solve_deal_single",
            "value": 220852464,
            "range": "± 516473739",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/32",
            "value": 4500488637,
            "range": "± 60472555",
            "unit": "ns/iter"
          },
          {
            "name": "solve_deals/200",
            "value": 28298826922,
            "range": "± 223216851",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/32",
            "value": 313232847,
            "range": "± 2955410",
            "unit": "ns/iter"
          },
          {
            "name": "solve_boards/200",
            "value": 1905270444,
            "range": "± 30550963",
            "unit": "ns/iter"
          },
          {
            "name": "analyse_plays_32",
            "value": 205444580,
            "range": "± 11189720",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}