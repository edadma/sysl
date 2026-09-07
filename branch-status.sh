#!/bin/bash
# Show ahead/behind status of all branches relative to dev and their remote
printf "%-10s %6s %6s  %s\n" "BRANCH" "+DEV" "-DEV" "REMOTE"
printf "%-10s %6s %6s  %s\n" "------" "----" "----" "------"
git -C /Users/ed/dev/sysl-lang/sysl-bootstrap for-each-ref \
  --format='%(refname:short) %(ahead-behind:dev) %(upstream:trackshort)' \
  refs/heads/ \
  | while read branch adev bdev remote; do
      printf "%-10s %6s %6s  %s\n" "$branch" "$adev" "$bdev" "${remote:-=-}"
    done
