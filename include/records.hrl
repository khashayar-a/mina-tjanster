%% All the records

-record(bank_account, {number, owner, transactions}).

-record(credit_card, {number, name, tansactions}).

-record(payment, {date, text, recipient, amount}).

-record(transaction, {date, text, amount}).



