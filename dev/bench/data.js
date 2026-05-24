window.BENCHMARK_DATA = {
  "lastUpdate": 1779658339962,
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
      }
    ]
  }
}