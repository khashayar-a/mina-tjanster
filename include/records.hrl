%% All the records

-record(bank_account, {number, owner, transactions}).

-record(credit_card, {number, name, transactions}).

-record(payment, {date, text, recipient, amount}).

-record(transaction, {date, text, amount}).



