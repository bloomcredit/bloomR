# bloomR

Use the Bloom Credit API with R.

## Using `{bloomR}`

#### Fetching Your Oauth2 Token

```
client_id <- "your_client_id""
client_secret <- "your_client_secret"

# fetch Oauth2 token
auth_token <- fetch_auth_token(
  audience = "dev",
  client_id = client_id,
  client_secret = client_secret
)
```

#### Registering A Consumer

- First, you will need to construct a list with all required fields.

```
consumer_info <- list(
  ssn = "123456789",
  city = "Scranton",
  line1 = "1725 Slough Avenue",
  state_code = "PA",
  zipcode = "18503",
  date_of_birth = "1964-03-15",
  first_name = "Michael",
  last_name = "Scott",
  address_primary = TRUE
)
```

- Next, you will need your `auth_token` to register the consumer.

```
consumer_id <- register_consumer(
  audience = "dev", 
  consumer_info = consumer_info, 
  auth_token = auth_token
  )
```

#### Placing An Order For A Credit Report

```
credit_order <- order_credit_data(
  audience = "dev", 
  consumer_id = consumer_id, 
  portfolio_id = "your_portfolio_id", 
  sku = "equifax-gold-soft-fico-internet", # or any other SKU that is available for your account.
  auth_token = auth_token
  )
```

#### Retrieving Credit Data

```
credit_data <- get_credit_data(
  audience = "dev",
  order_id = credit_order$data$id,
  auth_token = auth_token
)
```
