# Pine Intermediate VM Implementation Guide

## Detecting and Reporting Infinite Cycles

The VM is required to detect a number of distinct infinite-cycle shapes and to report them with an error message that explicitly mentions either "infinite loop" or "infinite recursion", states the cycle length, and includes a stack trace ending at the point where the cycle was entered. The check is amortized: the VM does not perform the (relatively expensive) check on every loop/jump/invocation, but only every 40,000 iterations of the main interpreter loop. In addition, the VM must check whenever it is about to return one of the configured limit errors, so that an infinite cycle is reported as such even when execution would otherwise hit a configured limit first.
