# "YAML specifications for target sets"
set_name: "Target Tagging Challenge" # required
set_version: "XXXX-XX-XX" # required
  # date of set creation/introduction/activation
  # this is solely used for differentiating sets of the same name
  # any time of completion restrictions are enforced by `during` parameters
note: "Free text" # optional

# requirements specifications
# if nothing is listed, then every target in the set must be:
# - reached once and only once
# - any time
#
# otherwise, need to define `when`
when: "any" # default
when: "any year"

when: "any winter" # or spring/summer/fall
when: "winter"

when: "any season" # needs season
when: "season" # needs season

when: "single year" # needs year
when: "single winter" # needs year

when: "single season" # needs season, year

when: "every season" # seasons grid, unrestricted
when: "every month" # month grid, unrestricted

when: "single every season" # seasons grid in one year, needs year
when: "single every month" # month grid in one year, needs year

# during parameter
# during: "any" # default - can be completed any time
# during: "year" # all in one year
# during: "winter" # all in a season
#
# # single parameter
# single: "no" # default - not restricted to a single `during` period
# single: "yes" # all in one `during` period
#   # if `during: "year"` then it is a calendar year challenge (i.e. fire tower)
#   # if `during: "winter"` then it is a single season challenge
#
# # every parameter - grids
# every: "no" # default - no restriction
# every: "season" # every target every season, like the Catskills Four Seasons
# every: "month" # every target every month, like the Catskills Grid
#
# # during + single + every - these *should* work, but are a bit crazy
# # for example, a single year every month grid
# during: "year"
# single: "yes"
# every: "month"
# # or a single year four seasons
# during: "year"
# single: "yes"
# every: "season"

# limit_to parameter - of the targets, only apply requirement to these
limit_to: "no" # default - no limits
limit_to:
  - "Panther"
  - "Blackhead"
  - "Slide"
  - "Balsam"

# needed parameter - how many of the listed peaks are needed
needed: "all" # default - all listed (or number in `limit_to` - are needed)
needed: 10 # need 10 of those listed, should validate that this is a number
needed: "10" # and that the number is > 0 and <= the total number of peaks

# complex `requirements` require a list/hash
# the (alphanumeic) sort order of the names will be the
#   the order that they are assessed/applied
# if the list includes a "vanilla" subset of do everything once anytime,
#.  there still needs to be an entry in the named sublist
#.  it can be anything valid, the suggestion is shown below (`during: "any"`)
# the alphanumeric sort ordering determines which requirement "gets" any
#   completions that could apply to more than one. Here, a completion that
#   could apply to the winter requirement will get captured by that requirement
#   before the general "core" do everything once requirement.
requirements:
  b_core:
    when: "any"
  a_winter:
    when: "winter"
    limit_to:
      - "Panther"
      - "Blackhead"
      - "Slide"
      - "Balsam"

