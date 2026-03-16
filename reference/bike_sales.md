# Fictional sales data for bike shops purchasing Cannondale bikes

A dataset containing the fictional bicycle orders spanning 2011 through
2015. Hypothetically, the `bike_sales` data are similar to sales data
mainatained in a business' sales data base. The unit price and model
names come from data provided by model for the bicycle manufacturer,
Cannondale (2016). The customers (bicycle shops) including name,
location, etc and the orders including quantity purchased and order
dates are fictional. The data is intended for implementing business
analytics techniques (e.g. forecast, clustering, etc) to identify
underlying trends.

## Usage

``` r
bike_sales
```

## Format

A data frame with 15644 rows and 17 variables:

- order.date:

  Date the order was placed

- order.id:

  A unique order identification number

- order.line:

  The sequential identification number for products on and order

- quantity:

  Number of units purchased

- price:

  The unit price of the bicycle

- price.ext:

  The extended price = price x quantity

- customer.id:

  A unique customer identification number

- bikeshop.name:

  The customer name

- bikeshop.city:

  The city that the bike shop is located

- bikeshop.state:

  The state that the bike shop is located

- latitude:

  The geograhpic latitude of the customer location

- longitude:

  The geograhpic longitude of the customer location

- product.id:

  A unique product identification number

- model:

  The model name of the bicycle

- category.primary:

  The main bicycle category, either "Mountain" or "Road"

- category.secondary:

  One of nine more specific bicycle categories

- frame:

  The bicycle frame material, either "Carbon" or "Aluminum"

## Source

The 2016 bicycle model names and prices originated from
<https://www.cannondale.com/en-us>
