library(dplyr)
library(ggplot2)

dataset <- read.csv("dataset_superstore_simple.csv")

dataset$sales <- as.numeric(dataset$sales)
dataset$sales[is.na(dataset$sales)] = 0

dataset$profit <- as.numeric(dataset$profit)
dataset$profit[is.na(dataset$profit)] = 0


#sales terbanyak
sales_trbnyk <- dataset%>%
  group_by(customer_id)%>%
  summarise(total_sales = sum(sales))%>%
  arrange(desc(total_sales))%>%
  head(1)
sales_trbnyk


#list sub category dalam Office Supplies
subcategory_office_supplies <- dataset%>% 
  filter(category == 'Office Supplies')%>%
  group_by(sub_category)%>%
  summarise(total_profit = sum(profit))%>%
  arrange(desc(total_profit))

subcategory_office_supplies

#Berapa banyak order yang menghasilkan profit negatif
profit_negatif <- dataset[dataset$profit < 0,]
profit_negatif%>% nrow()

#Sales terbanyak JE-16165, KH-16510, AD-10180
sales_terbanyak <- dataset%>% group_by(customer_id)%>%
  filter(customer_id %in% c("KH-16510","JE-16165","AD-10180"))%>%
  summarise(total_sales = sum(sales))%>%arrange(desc(total_sales))

sales_terbanyak

#yearly_sale,yearly_profit, yearly_customer_count
yearly_sales <- dataset
yearly_sales$order_date <- format(as.Date(dataset$order_date, format = "%Y-%m-%d"),"%Y")
yearly_sales$sales[is.na(yearly_sales$sales)] = 0

total_yearly <- yearly_sales%>%
  group_by(order_date)%>%
  summarise(
    total_sales = sum(sales), 
    total_profit = sum(profit),
    customer_count = n())%>%
  arrange(desc(total_profit))

total_yearly

#scater plot
scatter_plot <- dataset
scatter_plot$order_date <- format(as.Date(dataset$order_date, format = "%Y-%m-%d"),"%Y")
scatter_plot <- scatter_plot%>%filter(order_date %in% c("2014","2015"))

scatter_sales_profit <- ggplot(scatter_plot, aes(x=sales, y=profit)) + 
  geom_point(aes(color = order_date), size = 0.5, shape = 17) + 
  scale_colour_discrete("Order Date")+
  geom_smooth(method = 'lm')+
  labs(title = 'Sales vs Profit 2014-2015', 
       caption = 'Herpaniel Rumende Mangeka',
       fill = 'Order Date'
       ) +
  xlab("Sales") + ylab("Profit") +
  xlim(-1000,10000) + ylim(-1000,2500) +
  theme(
    plot.title = element_text(hjust = 0.5, size=11,face="bold.italic"),
    legend.position = c(0.91,0.14),
    legend.title= element_text(color = "Black",size=8,face="bold"),
    legend.text = element_text(color = "#0f540d",face="bold")
    )
scatter_sales_profit
ggsave("Sales vs Profit 2014-2015.png",scatter_sales_profit)
 
#top 10 top sales customer 
top_ten_customer <- dataset

top_ten_customer$order_date <- format(as.Date(dataset$order_date, 
                                              format = "%Y-%m-%d"),"%Y")

top_ten_customer <- top_ten_customer%>% 
  filter(order_date == "2015")

top_ten_customer <- top_ten_customer %>%
  group_by(customer_id)%>% 
  summarise(total_sales = sum(sales), 
            total_profit = sum(profit))

top_ten_customer <-  top_ten_customer%>%
  arrange(desc(total_sales))%>% 
  head(10)

top_ten_customer <-  top_ten_customer%>%
  select(c(customer_id, total_profit))%>% arrange(desc(total_profit))

top_ten_customer


#bar chart top 10 sales per customer
bar_chart_sales <- ggplot(top_ten_customer, aes(x = reorder(customer_id, -total_profit), 
                             y = total_profit))+
  geom_bar(
    stat = 'identity', 
    width = 0.6,
    aes(fill = customer_id),
    ) + 
  labs(title = 'Top 10 Customer Profit by Highest Sales', 
       caption = 'Herpaniel Rumende Mangeka',
       fill = 'Customer ID'
  )+
  geom_text(aes(label = total_profit), size = 4)+
  theme(
    plot.title = element_text(hjust = 0.5, size=13,face="bold.italic"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  )+
  xlab("Customer ID") + ylab("Total Profit") + geom_line()
bar_chart_sales
ggsave("top sales by customer.png",bar_chart_sales,width = 8, height = 8)
