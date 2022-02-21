ec2_mock
=====

An escript

Build
-----

    $ rebar3 escriptize


Setup
---

    On ec2_instance you want to emulate:

    (python script requires requests library: pip install requests)

    $ ec2_metadata.py, copy output to priv/metadata.json on ec2_mock instance.  Edit as needbe.

Run
---

    $ _build/default/bin/ec2_mock
