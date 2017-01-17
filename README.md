# mina-tjanster

## How to run
### Compile and run program
  `rebar compile`
  
  `erl -pa ebin -eval "application:start(bank)"`
### Compile and run test
  `rebar compile eunit`
  
## API
### Fetch outgoing transactions
`bank_server:outgoing_transactions()`

### Calculate balance 
`bank_server:calculate_balance(Bank_Account_Number).`

example: `bank_server:calculate_balance(2).`
    
### Detect interval
`bank_server:detect_interval(Bank_Account_Number, Text).`

example: `bank_server:detect_interval(2, "Gym").`
  
## Utilities
### Load data
There is functionality for loading data from text file. Text file should be comma separated. For examples you can look at [this sample text file](../master/include/sample_bank_account.txt).
You can use this as:

`bank_ets:load_bank_accounts_transaction(Account_Number, Owner, FILE).`
or
`bank_ets:load_credit_cards_transaction(Number, Name, FileName)`

example: `bank_ets:load_bank_accounts_transaction(2, "Khash", "include/sample_bank_account.txt").`

## Assumptions

- Format of the file to load data from is comma separated.
- A payment is reoccuring if it occurs more thant 2 times.
