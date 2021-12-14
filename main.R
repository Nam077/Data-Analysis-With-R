library(shinydashboard)
library(ggplot2)
library(DT)
library(gridExtra)
library(dplyr)
library(tidyverse)
library(tree)
library(shiny)
library(pacman)
library(caret)
library(corrplot)
library(RColorBrewer)
library("caTools")
path <- 'F:\\dataset\\index.csv'
data <- read.csv(path, sep = ",")
str(data)
data <- add_column(data, price2 = log(data$price) , .after = "price")
colnames(data)
data2 = data
data2["ID"] = c(1:21613)
mycolors = c(brewer.pal(name="Dark2", n = 8))
# Rút trích ra chỉ lấy năm của biến date (ngày bán)
data2$date = substr(data2$date, 1, 4)
# Chuyển thành dạng numeric để có thể biểu diễn mối tương quan giữa các biến (correlation)
data2$date = as.numeric(as.character(data2$date))
#thêm biến tuổi nhà 
data2$house_age <- data2$date - data2$yr_built
# Xoá biến yr_built
data2$yr_built = NULL
#id: ID nhà
#date: ngày mà căn nhà đó được bán.
#price: giá nhà
#bedrooms: số phòng ngủ
#bathrooms: số nhà tắm
#sqft_living: Square foot Living ( diện tích không gian sống bên trong nhà)
#sqft_lot: Square foot Lot (diện tích sân trước và sân sau )
#floors: số tầng
#waterfront: có bên cạnh sông, biển hay không
#view: Chất lượng khung nhìn
#condition: điều kiện căn nhà ( xếp từ 1->5 sao)
#grade: hạng mục nhà (1->13 hạng mục)
#sqft_above: Square foot above ( diện tích không gian sống từ tầng trệt trở lên mà không tính tầng hầm)
#sqft_basement: Square foot basement (diện tích tầng hầm)
#yr_built: năm ngôi nhà được xây
#yr_renovated: năm ngôi nhà được sửa sang
#zipcode: mã bưu điện
#lat: latitude (vĩ độ)
#long: longitude (kinh độ)
#sqft_living15: Square foot Living trung bình của 15 căn nhà gần kề
#sqft_lot15: Square foot Lot trung bình của 15 căn nhà gần kề

df1 <- data.frame(table(data2$zipcode))
City <- array(df1$Var1)
A_price <- c()
i = 1
while (i <= length(City)) {
  data3 <- subset(data2, zipcode == zipcode[i])
  a_p <- mean(data3$price) / mean(data3$sqft_living)
  A_price <- c(A_price, c(a_p))
  i = i +1
}
data3 <- data.frame(City, A_price)
data3
view(data3)
city = data3[which(data3$A_price == max(data3$A_price)),]
city
city = data3[which(data3$A_price == min(data3$A_price)),]
city

ggplot(data=data3, aes(x=A_price, y=City)) + geom_bar(stat="identity")

numberOfNA = length(which(is.na(data) == T))
print(numberOfNA)
if(numberOfNA > 0)
{
  cat('Number of missing values: ', numberOfNA)
  cat('\nRemoving missing values...')
  data = data[complete.cases(data), ]
}

df = data2
discreteFeat <- c("waterfront", "view", "condition", "grade")
df[,discreteFeat] <- lapply(df[,discreteFeat], as.factor)
t(sapply(df, class))
# In ra số lượng nhà mà chưa từng được sửa sang (Yr_renovated =0) 
length(df$yr_renovated[df$yr_renovated == 0])
# Tạo ra 1 biến mới renovated = 1 khi đã từng sửa sang và =0 khi chưa từng sửa sang
df$renovated <- ifelse(df$yr_renovated == 0, 0, 1)
# Chuyển biến category renovated sang dạng factor
df$renovated = as.factor(df$renovated)
# Xoá biến yr_renovated
df$yr_renovated = NULL
# In ra số lượng nhà mà có tầng hầm (sqft_basement = 0) 
length(df$sqft_basement[df$sqft_basement == 0])
# Tạo ra 1 biến mới basement = 1 với nhà có tầng hồm và =0 cho nhà không có tầng hầm
df$basement <- ifelse(df$sqft_basement == 0, 0, 1)
# Chuyển biến category basement sang dạng factor
df$basement = as.factor(df$basement)
# Xoá biến sqft_basement
df$sqft_basement = NULL

#     |-------------------------------------------|
#     |               Xây dụng mô hình            |
#     |-------------------------------------------|
split = sample.split(df$ID, SplitRatio = 2/3)
mauXayDung = subset(df, split==TRUE)
mauKiemDinh = subset(df, split==FALSE)
#####################################################################$=##################
#Tạo một mô hình hồi quy đơn giản Diện tích ở với giá thành
moHinh1 = lm(price ~ sqft_living, data = mauXayDung)
moHinh1
summary(moHinh1)
#Ý nghĩa của bảng số summary
#Estimate: Hệ số beta của mô hình hồi quy
#Std. Error: Độ lệch chuẩn của ước lượng beta tương ứng
#t-value Estimate/Std. Error làn  giá trị t trong kểm định giả thuyết
#Với H0:bet = 0 , H1: beta<>0, nếu giá trị t có trị tuyệt đối
#lớn hơn giá trị tới hạn giả thiết H- bijbacs bỏ , thể hiện số beta có giá trị 
#Cột Pr(>|t|) gí trị b-value trong kiểm định giả thuyết H0: beta = 0, H1: beta<>0
#Dự báo giá trị của price trong bộ mẫu xanh dựng sử dụng hàm predict
duBao_xayDung = predict(moHinh1, mauXayDung)
#Tổng số dư bình phương (RSS) của bộ mẫu xây dựng
RSS_xayDung = sum((duBao_xayDung - mauXayDung$price) ^ 2)
RSS_xayDung
#Tổng số dư bình phương (RSS) của bộ mẫu cơ sở
TSS_xayDung = sum((mean(mauXayDung$price) - mauXayDung$price) ^ 2)
TSS_xayDung
#Giá trị R-square(R2) của mô hình mẫu xây dựng
R2_xayDung = 1 - (RSS_xayDung / TSS_xayDung)
R2_xayDung
#Thực hiện tương tự để tính R2 cho mẫu kiểm định cú ý là khi tính TSS cho mẫu kiểm định
duBao_kiemDinh = predict(moHinh1, mauKiemDinh)
RSS_kiemDinh = sum((duBao_kiemDinh - mauKiemDinh$price) ^ 2)
RSS_kiemDinh
TSS_kiemDinh = sum((mean(mauXayDung$price) - mauKiemDinh$price) ^ 2)
R2_kiemDinh = 1 - (RSS_kiemDinh / TSS_kiemDinh)
R2_kiemDinh
house1 = df[which(df$ID == '430'),]
house1$price
predict(moHinh1, house1)
#-----------------------Xây dựng mô hình hồi quy tuyến tính 2------------------------------------
moHinh2 = lm(price~. -id - ID - price2 - zipcode, data = mauXayDung)
moHinh2
summary(moHinh2)
duBao_xayDung = predict(moHinh2, mauXayDung)
RSS_xayDung = sum((duBao_xayDung - mauXayDung$price) ^ 2)
TSS_xayDung = sum((mean(mauXayDung$price) - mauXayDung$price) ^ 2)
R2_xayDung = 1 - (RSS_xayDung / TSS_xayDung)
duBao_kiemDinh = predict(moHinh2, mauKiemDinh)
RSS_kiemDinh = sum((duBao_kiemDinh - mauKiemDinh$price) ^ 2)
TSS_kiemDinh = sum((mean(mauXayDung$price) - mauKiemDinh$price) ^ 2)
R2_kiemDinh = 1 - (RSS_kiemDinh / TSS_kiemDinh)

#-----------------------Mô hình cây-------------------------
# Hàm tree dùng để tạo ra cây phân loại giúp dự đoán giá trị đầu ra là biến price và sử dụng tất cả các biến còn lại làm giá trị đầu vào trong dữ liệu tập train
tree.fit = tree(price~. -price2,data = mauXayDung)
summary(tree.fit)
tree.fit
plot(tree.fit)
text(tree.fit ,pretty =0)

# Thực nghiệm mô hình Generalized Linear Regression với biến price là biến đầu price và các biến còn lại làm biến đầu vào.
moHinh3 = glm(price~., data=mauXayDung)
summary(moHinh3)
house1$price
predmodel2 <- predict(moHinh3, mauXayDung)
act_pred <- data.frame(obs=log(mauXayDung$price), pred=predmodel2)
defaultSummary(act_pred)
predict(moHinh2, house1)
predmodel2 <- predict(glm.fit, mauKiemDinh)
act_pred <- data.frame(obs=log(mauKiemDinh$price), pred=predmodel2)

print(defaultSummary(act_pred))
#     |-------------------------------------------|
#     |                   Giao Diện               |
#     |-------------------------------------------|
ui <- dashboardPage(
  dashboardHeader(title = "Phân tích giá nhà"),
  dashboardSidebar(),
  dashboardBody(
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("edit")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Tính Toán", tabName = "math", icon = icon("question")),
        menuItem("Mô hình hồi quy đơn giản", tabName = "model1", icon = icon("house-damage")),
        menuItem("Mô hình hồi quy tất cả các biến", tabName = "model2", icon = icon("house-damage")),
        menuItem("Dự đoán giá nhà", tabName = "houseprice", icon = icon("home"))
      )
    ),
    
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      
      tabItem(tabName = "dashboard",
              wellPanel(
                sliderInput("dataselect", "Chọn số lượng dữ liệu", min(data2$ID), max(data2$ID),
                            value = c( min(data2$ID), max(data2$ID)), step = 1)
              ),
              fluidRow(
                column(2,
                       selectInput("zipcode2",
                                   "Mã Thành Phố:",
                                   c("All",
                                     unique(as.character(data2$zipcode))))
                ),
              ),
              
              fluidRow(
                box(plotOutput("plot1")),
                box(plotOutput("plot2"))
              ),
              fluidRow(
                box(plotOutput("plot3")),
                box(plotOutput("plot4"))
              ),
              fluidRow(
                box(plotOutput("plot5")),
                box(plotOutput("plot6"))
              ),
              fluidRow(
                box(plotOutput("plot7")),
                box(plotOutput("plot8"))
              ),
              fluidRow(
                box(plotOutput("plot9")),
                box(plotOutput("plot10"))
              ),
              fluidRow(
                box(plotOutput("plot11")),
                box(plotOutput("plot12"))
              ),
              fluidRow(
                box(plotOutput("plot13")),
                box(plotOutput("plot14"))
              ),
              fluidRow(
                box(plotOutput("plot15")),
                box(plotOutput("plot16"))
              ),
              fluidRow(
                box(plotOutput("plot17")),
                box(plotOutput("plot18"))
              ),
              
      ),
      tabItem(tabName = "data",
              titlePanel("Basic DataTable"),
              
              # Create a new Row in the UI for selectInputs
              # Create a new row for the table.
              DTOutput('table')
      ),
      tabItem(tabName = "model1",
              titlePanel("Mô hình hồi quy đơn giản"),
              fluidRow(
                box(plotOutput("simplemodel")),
                box(plotOutput("simplemodel2"))
              )
      ),
      tabItem(tabName = "model2",
              titlePanel("Mô hình hồi quy tất cả các biến"),
              plotOutput("model2", height = 1000),
              plotOutput("model2a")
      ),
      tabItem(tabName = "math",
              
              titlePanel("Tính toán trong bài"),
              fluidRow(
                infoBoxOutput("total"),
                infoBoxOutput("total1"),
                infoBoxOutput("total2"),
                infoBoxOutput("total3"),
                infoBoxOutput("total4"),
                infoBoxOutput("total5"),
                infoBoxOutput("total6"),
                infoBoxOutput("total7"),
                infoBoxOutput("total8"),
                infoBoxOutput("total10"),
                infoBoxOutput("total11"),
                infoBoxOutput("total12"),
                infoBoxOutput("total13"),
                
              ),
              
              wellPanel(
                sliderInput("pricehouse", "Chọn khoảng giá", min(data2$price), max(data2$price),
                            value = c( min(data2$price), max(data2$price)), step = 50)
              ),
              wellPanel(
                sliderInput("sqft_living", "Chọn diện tích ở", min(data2$sqft_living), max(data2$sqft_living),
                            value = c( min(data2$sqft_living), max(data2$sqft_living)), step = 50)
              ),
              wellPanel(
                sliderInput("sqft_lot", "Chọn diện tích lô", min(data2$sqft_lot), max(data2$sqft_lot),
                            value = c( min(data2$sqft_lot), max(data2$sqft_lot)), step = 50)
              ),
              wellPanel(
                sliderInput("sqft_above", "Chọn Diện tích tầng trên", min(data2$sqft_above), max(data2$sqft_above),
                            value = c( min(data2$sqft_above), max(data2$sqft_above)), step = 50)
              ),
              wellPanel(
                sliderInput("sqft_basement", "Chọn Diện tích tầng dưới", min(data2$sqft_basement), max(data2$sqft_basement),
                            value = c( min(data2$sqft_basement), max(data2$sqft_basement)), step = 50)
              ),
              fluidRow(
                column(2,
                       selectInput("zipcode",
                                   "Mã Thành Phố:",
                                   c("All",
                                     unique(as.character(data2$zipcode))))
                ),
                column(2,
                       selectInput("bedrooms",
                                   "Phòng Ngủ:",
                                   c("All",
                                     unique(as.character(data2$bedrooms))))
                ),
                column(2,
                       selectInput("bathrooms",
                                   "Phòng Tắm:",
                                   c("All",
                                     unique(as.character(data2$bathrooms))))
                ),
                column(2,
                       selectInput("condition",
                                   "Tình Trạng:",
                                   c("All",
                                     unique(as.character(data2$condition))))
                ),
                column(2,
                       selectInput("yr_built",
                                   "Năm xây dựng:",
                                   c("All",
                                     unique(as.character(data2$yr_built))))
                ),
                column(2,
                       selectInput("floors",
                                   "Số tầng lầu:",
                                   c("All",
                                     unique(as.character(data2$floors))))
                ),
                
              ),
              # Create a new row for the table.
              DT::dataTableOutput("pricetable")
      ),
      tabItem(tabName = "houseprice",
              titlePanel("Mô hình hồi quy tất cả các biến"),
              wellPanel(
                sliderInput("slideID", "Chose Data", 0, length(data2$ID),
                            value = 2814, step = 1)
              ),
              DTOutput('house'),
              fluidRow(
                box(textOutput("price1")),
                box(textOutput("price1a"))
              ),
              fluidRow(
                box(textOutput("price2")),
                box(textOutput("price2a"))
              ),
              fluidRow(
                box(textOutput("price3")),
                box(textOutput("price3a"))
              )
      ),
      
      
      tabItem(tabName = "graph",
              verticalLayout(
                wellPanel(
                  sliderInput("slider", "Chose Data", 0, length(data2$ID),
                              value = 30, step = 1)
                ),
                
              )
              
              # Create a new Row in the UI for selectInputs
              # Create a new row for the table.
              
      ),
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox"),
                infoBox(
                  "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
                  color = "yellow", fill = TRUE
                )
              ),
              
              # infoBoxes with fill=TRUE
              fluidRow(
                infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                infoBoxOutput("progressBox2"),
                infoBoxOutput("approvalBox2")
              ),
              
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4, actionButton("count", "Increment progress"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm
  dataselect <- reactive({
    datau = data2[data2$ID >= input$dataselect[1] & data2$ID <= input$dataselect[2],]
    if (input$zipcode2 != "All") {
      datau <- datau[datau$zipcode == input$zipcode2,]
    }
    datau
  })
  pricedata <- reactive({
    data5 = data2[data2$price >= input$pricehouse[1] & data2$price <= input$pricehouse[2],]
    data5 = data5[data5$sqft_basement >= input$sqft_basement[1] & data5$sqft_basement <= input$sqft_basement[2],]
    data5 = data5[data5$sqft_above >= input$sqft_above[1] & data5$sqft_above <= input$sqft_above[2],]
    data5 = data5[data5$sqft_lot >= input$sqft_lot[1] & data5$sqft_lot <= input$sqft_lot[2],]
    data5 = data5[data5$sqft_living >= input$sqft_living[1] & data5$sqft_living <= input$sqft_living[2],]
    if (input$zipcode != "All") {
      data5 <- data5[data5$zipcode == input$zipcode,]
    }
    if (input$bedrooms != "All") {
      data5 <- data5[data5$bedrooms == input$bedrooms,]
    }
    if (input$bathrooms != "All") {
      data5 <- data5[data5$bathrooms == input$bathrooms,]
    }
    if (input$floors != "All") {
      data5 <- data5[data5$floors == input$floors,]
    }
    if (input$yr_built != "All") {
      data5 <- data5[data5$yr_built == input$yr_built,]
    }
    if (input$condition != "All") {
      data5 <- data5[data5$condition == input$condition,]
    }
    data5
  })
  output$pricetable <- renderDT(pricedata(), class = 'cell-border stripe',
                                
                                options = list(
                                  pageLength = 8, scrollX = TRUE,
                                  stringsAsFactors = FALSE
                                )
  )
  
  
  
  output$price1 <- renderText({ 
    house = housedata2()
    paste("Giá :", house$price)
  })
  output$price1a <- renderText({ 
    house = housedata2()
    s <- predict(moHinh1, house)
    paste("Giá đoán mô hình 1 :", round(s, digits = 0))
  })
  output$price2 <- renderText({ 
    house = housedata2()
    paste("Giá :", house$price)
  })
  output$price2a <- renderText({ 
    house = housedata2()
    s <- predict(moHinh2, house)
    paste("Giá đoán mô hình 2 :", round(s, digits = 0))
  })
  output$price3 <- renderText({ 
    house = housedata2()
    paste("Giá :", house$price)
  })
  output$price3a <- renderText({ 
    house = housedata2()
    s <- predict(moHinh3, house)
    paste("Giá đoán mô hình 3 :", round(s, digits = 0))
  })
  housedata <- reactive({
    data3 =  data2[data2$ID == input$slideID,]
  })
  housedata2 <- reactive({
    house = df[which(df$ID == input$slideID),]
  })
  output$table <- renderDT(data, class = 'cell-border stripe',
                           filter = "top",
                           options = list(
                             pageLength = 8, scrollX = TRUE,
                             stringsAsFactors = FALSE
                           )
  )
  output$house <- renderDT(housedata(), class = 'cell-border stripe',
                           
                           options = list(
                             pageLength = 1, scrollX = TRUE,
                             stringsAsFactors = FALSE
                           )
  )
  #Biến right có dấu hiệu lệch khi mà các giá trị của biến price quá cao 
  #và tập trung thành 1 cụm khiến cho chúng ta khó mà quan sát rõ được sự phân bổ của nó.
  output$plot1 <- renderPlot({
    ggplot(dataselect(), aes(x=price)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FF6666")  + ggtitle("Biểu đồ Histogram biểu diễn sự phân bổ của biến price")
  })
  #Ta sẽ lấy log biến price để chuyển sang dạng phân phối chuẩn (normal distribution)
  output$plot2 <- renderPlot({
    ggplot(dataselect(),aes(x=log(price)))+geom_histogram(alpha = 0.8,fill="tomato4")+
      labs(x= 'Price',y = 'Count', title = paste("Distribution of", ' Price ')) +
      theme_bw()+ggtitle("Biểu đồ Histogram biểu diễu sự phân bố biến price")
    
  })
  #Biến đầu vào sqft_living và biến đầu ra price có một quan hệ tuyến tính rõ rệt, 
  #biến sqft_living và price có giá trị khá lớn và có dấu hiệu lệch nên 
  #đã biến đổi thành dạng normal để quan sát dữ liệu rõ ràng hơn
  #Sử dụng geom_jitter để tạo ra các điểm dữ liệu màu nâu và sử dụng stat_smooth để tạo ra đường hồi quy tuyến tính fit các điểm dữ liệu với nhau
  output$plot3 <- renderPlot({
    ggplot(dataselect(),aes(x=log(sqft_living),y=log(price)))+geom_jitter(alpha=0.5,size=2,color="brown")+stat_smooth(method="lm",se=F,span=0.7)+labs(title="Sqft Living vs Price")+
      theme_bw()+ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến sqft_living và biến Price")
  })
  # Ggplot biểu diễn mối quan hệ giữa biến Bathrooms và biến Price, sử dụng geom_jitter để tạo ra các điểm dữ liệu và sử dụng stat_smooth để tạo ra đường hồi quy tuyến tính fit các điểm dữ liệu với nhau
  
  # Chuyển đổi màu màu của các điểm thành 8 loại màu khác nhau sử dụng brewer.pal
  #Số lượng bathrooms càng nhiều, giá cả càng cao
  output$plot4 <- renderPlot({
    ggplot(dataselect(),aes(x=bathrooms,y=log(price),col=bathrooms))+ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến Bathrooms và biến Price")+
      geom_point(alpha=0.5,size=2)+
      geom_smooth(method="lm",se=F)+scale_color_gradientn(colors=mycolors) + theme_bw() + theme(legend.position="none")
    
    
  })
  ## Ggplot sự phân bố biến bathrooms, với trục x là số lượng nhà tắm 
  #và trục y là biến count đếm số lượng giá trị đó
  output$plot5 <- renderPlot({
    ggplot(dataselect(),aes(x=bathrooms))+geom_histogram(alpha=0.8,fill="green4",binwidth=0.5)+
      labs(x= 'Bathrooms',y = 'Count', title = paste("Distribution of", ' Bathrooms ')) +
      theme_bw() + ggtitle("Ggplot sự phân bố biến bathrooms")
    
  })
  output$plot6 <- renderPlot({
    ggplot(dataselect(),aes(x=bedrooms,y=log(price),col=bedrooms))+
      geom_point(alpha=0.5,size=2)+
      geom_smooth(method="lm",se=F)+
      labs(title="Bedrooms vs Price")+scale_color_gradientn(colors=mycolors) + theme_bw() + theme(legend.position="none")
  })
  #lọc dữ liệu loại bỏ các ngôi nhà có trên 30 phòng tắm
  datafilter <- reactive({
    datafilter = dataselect()
    datafilter =  datafilter[datafilter$bedrooms < 30, ]
  })
  #Các ngôi nhà có giá cao nhất hầu hết đều có từ 5,6 nhà tắm.
  output$plot7 <- renderPlot({
    ggplot(datafilter(), aes(x=bedrooms,y=log(price),col=bedrooms))+
      geom_point(alpha=0.5,size=2)+
      geom_smooth(method="lm",se=F)+
      labs(title="Bedrooms vs Price")+scale_color_gradientn(colors=mycolors)+ theme_bw() + theme(legend.position="none")
  })
  # Ggplot biểu diễn mối quan hệ giữa biến lat và biến Price, 
  #sử dụng geom_jitter để tạo ra các điểm dữ liệu và sử dụng stat_smooth để 
  #tạo ra đường hồi quy tuyến tính fit các điểm dữ liệu với nhau
  #Ta thấy có một mối quan hệ tuyến tính giữa biến price và biến lat,
  #giá nhà cao nhất khi lat nằm trong khoảng 47.6 tới 47.8 với price có thể lên tới e^15 tới e^16 $
  output$plot8 <- renderPlot({
    ggplot(dataselect(),aes(x=lat,y=log(price),col=lat))+
      geom_point(alpha=0.5,size=2)+
      ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến lat và biến Price,")+
      geom_smooth(method="lm",se=F)+
      scale_color_gradientn(colors=mycolors) + 
      theme_bw() + theme(legend.position="none")
  })
  # Ggplot biểu diễn mối quan hệ giữa biến sqft_basement và biến Price, 
  #sử dụng geom_point để tạo ra các điểm dữ liệu 
  #và sử dụng stat_smooth để tạo ra đường hồi quy tuyến tính fit các điểm dữ liệu với nhau
  #Ta thấy rất nhiều ngôi nhà không có tầng hầm và tập trung lại thành 1 cụm rất lớn.
  output$plot9 <- renderPlot({
    ggplot(dataselect(),aes(x=sqft_basement,y=log(price)))+
      geom_point(col="green",alpha=0.5)+ labs(title="Sqft_basement vs Price")+
      ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến sqft_basement và biến Price")+
      stat_smooth(method="lm",se=F,alpha=0.6,size=0.5)+ theme_bw()
  })
  # Ggplot biểu diễn mối quan hệ giữa biến condition và biến Price
  # Sử dụng geom_boxplot để biểu diễn dưới dạng boxplot, scale_fill_manual dùng để thay đổi màu sắc các boxplot
  output$plot10 <- renderPlot({
    ggplot(dataselect(),aes(factor(condition),log(price),fill=factor(condition)))+
      geom_boxplot(alpha=0.6)+scale_fill_manual(values=rainbow(6))+
      theme(legend.position="none")+ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến condition và biến Price") +
      labs(x="House Condition") + theme_bw()
  })
  # Sử dụng geom_boxplot để biểu diễn dưới dạng boxplot, scale_fill_manual dùng để thay đổi màu sắc các boxplot
  output$plot11 <- renderPlot({
    ggplot(dataselect(),aes(factor(floors),log(price),fill=factor(floors)))+geom_boxplot(alpha=0.5)+
      labs(x = "Floors", y = "logprice")+
      ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến floors và biến Price")+
      scale_fill_manual(values=rainbow(n=8))+
      theme_bw()
  })
  #Ta thấy rõ là ngôi nhà càng có tầm nhìn tốt thì giá thành càng cao, hầu hết các căn nhà đều có tầm nhìn không được tốt.
  output$plot12 <- renderPlot({
    ggplot(dataselect(),aes(factor(view),log(price),fill=factor(view)))+geom_boxplot(alpha=0.5)+
      ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến view và biến Price")+
      labs(x = "View", y = "logprice")+
      scale_fill_manual(values=rainbow(n=5))+
      theme_bw()
  })
  #Những ngôi nhà thấy được sông hồ giá thành mắc hơn, hầu hết ngôi nhà đều không có sông hồ.
  output$plot13 <- renderPlot({
    ggplot(dataselect(), aes(x = factor(waterfront), y = log(price),fill=factor(waterfront))) +
      ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến waterfront và biến Price")+
      geom_boxplot(alpha=0.5) +
      labs(x = "waterfront", y = "logprice")+
      scale_fill_manual(values=rainbow(n=12))
  })
  #Ta thấy hạng mục ngôi nhà càng cao thì giá thành càng cao, hầu hết các ngôi nhà đều tập trung ở hạng mục 6->9
  output$plot14 <- renderPlot({
    ggplot(dataselect(),aes(factor(grade),log(price),fill=factor(grade)))+geom_boxplot(alpha=0.5)+
      ggtitle("Biểu đồ biểu diễn mối quan hệ giữa biến grade và biến Price")+
      labs(x = "Grade", y = "logprice")+
      scale_fill_manual(values=rainbow(n=13))+
      theme_bw()
  })
  output$plot16 <- renderPlot({
    data20 = dataselect()
    corr = cor(subset(data20,select = -c(id,date)))
    corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))
  })
  output$plot17 <- renderPlot({
    data20 = dataselect()
    qplot(data20$sqft_living,data20$price2,colour=data20$price2,
          xlab ="Squre feet", ylab="price($)")
  })
  output$plot15 <- renderPlot({
    data20 = dataselect()
    ggplot(data20,aes(x=house_age,y=log(price),col=house_age))+
      geom_point(alpha=0.5,size=2)+
      geom_smooth(method="lm",se=F)+
      labs(title="HouseAge vs Price")+scale_color_gradientn(colors=mycolors) + theme_bw() + theme(legend.position="none")
  })
  
  output$simplemodel <- renderPlot({
    ggplot(mauXayDung, aes(x = sqft_living, y = price,)) +
      geom_point(alpha = 1, colour = 'blue') + xlab("Diện tích ở") + ylab("Giá") +
      geom_abline(intercept = moHinh1$coefficient[1], slope = moHinh1$coefficients[2], colour = "red") + ggtitle("Biểu đồ mô hình hồi quy với tập xây dựng")
  })
  output$simplemodel2 <- renderPlot({
    ggplot(mauKiemDinh, aes(x = sqft_living, y = price,)) +
      geom_point(alpha = 1, colour = 'blue') + xlab("Diện tích ở") + ylab("Giá") +
      geom_abline(intercept = moHinh1$coefficient[1], slope = moHinh1$coefficients[2], colour = "red") + ggtitle("Biểu đồ mô hình hồi quy với tập thử")
  })
  output$model2 <- renderPlot({
    par(mfrow=c(2,2))
    plot(moHinh2)
  })
  output$model2a <- renderPlot({
    par(mfrow = c(1, 2))
    plot(moHinh2, which = c(4, 6))
  })
  output$total <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Tổng số ngôi nhà: ", length(data6$ID),
      color = "green", fill = TRUE
    )
  })
  output$total1 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình giá nhà: ",round(x = mean(data6$price),digits = 0) ,
      color = "red", fill = TRUE
    )
  })
  output$total2 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình số phòng ngủ: ",mean(data6$bedrooms) , icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  output$total3 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình số phòng ăn: ",mean(data6$bathrooms) , icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  maxprice <- reactive({
    data6 = pricedata()
    house = data6[which(data6$price == max(data6$price)),]
  })
  output$total4 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình diện tích ở: ",mean(data6$sqft_living) , icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  minprice <- reactive({
    data6 = pricedata()
    house = data6[which(data6$price == min(data6$price)),]
  })
  output$total5 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Diện tích không gian sống rộng nhất: ", max(data6$sqft_living),
      color = "green", fill = TRUE
    )
  })
  output$total6 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Diện tích không gian sống nhỏ nhất: ", min(data6$sqft_living                                                                                                                                                                                                                                      ),
      color = "green", fill = TRUE
    )
  })
  output$total7 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình diện tích sân: ", mean(data6$sqft_lot                                                                                                                                                                                                                              ),
      color = "green", fill = TRUE
    )
  })
  output$total8 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình diện tích tầng 1 trở lên: ", mean(data6$sqft_above                                                                                                                                                                                                                              ),
      color = "green", fill = TRUE
    )
  })
  output$total9 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Trung bình diện tích tầng hầm: ", mean(data6$sqft_basement                                                                                                                                                                                                                               ),
      color = "green", fill = TRUE
    )
  })
  output$total10 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Diện tích tầng hầm nhỏ nhất: ", min(data6$sqft_basement                                                                                                                                                                                                                               ),
      color = "green", fill = TRUE
    )
  })
  output$total11 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Diện tích tầng hầm lớn nhất: ", max(data6$sqft_basement                                                                                                                                                                                                                               ),
      color = "green", fill = TRUE
    )
  })
  output$total12 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Diện tích tầng 1 trở lên nhỏ nhất: ", min(data6$sqft_above                                                                                                                                                                                                                               ),
      color = "green", fill = TRUE
    )
  })
  output$total13 <- renderInfoBox({
    data6 = pricedata()
    infoBox(
      "Diện tích tầng 1 trở lên lớn nhất: ", max(data6$sqft_above                                                                                                                                                                                                                               ),
      color = "green", fill = TRUE
    )
  })
}

shinyApp(ui, server)

