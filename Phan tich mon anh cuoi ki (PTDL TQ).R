library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(cowplot)



data_2022 = read_csv("D:/Phân tích dữ liệu trực quan/diem_thi_thpt_2022.csv")
data_2022

data_2021 = read.csv("D:/Phân tích dữ liệu trực quan/diem_thi_thpt_2021.csv")
data_2021

#gán biến ngoai ngu 
anhvan_2022 = data_2022$ngoai_ngu
anhvan_2022

anhvan_2021 = data_2021$ngoai_ngu
anhvan_2021


cat("Tổng số học sinh đăng kí thi môn ngoại ngữ năm 2022:", length((anhvan_2022)))
cat("Tổng số học sinh đăng kí thi môn ngoại ngữ năm 2021:", length((anhvan_2021)))
995441 - 124832
934618 - 116447
summary(anhvan_2022)
#summary môn anh ta thấy được trung bình các thí sinh làm được là 5.16 điểm

summary(anhvan_2021)
#summary môn anh ta thấy được trung bình các thí sinh làm được là 5.86 điểm


# vì còn có các giá trị NA nên ta sẽ loại bỏ các giá trị này để tính mean và sd chính xác hơn
# loại bỏ các giá trị NA của môn anh
loc_AV_2022 <- filter(data_2022, !is.na(ngoai_ngu))
loc_AV_2022
loc_AV_2021 <- filter(data_2021, !is.na(ngoai_ngu))
loc_AV_2021

#na.rm == None Available remove, tính mean và độ lệch chuẩn bỏ các giá trị NA
# khi dùng hàm cat sẽ trả về kết quả cùng hàng mà ko cần xuống hàng như hàm print 
## Tính mean 
mean2022 <- mean(loc_AV_2022$ngoai_ngu, na.rm = TRUE)
cat("Mean-2022:", mean2022)

mean2021 <- mean(loc_AV_2021$ngoai_ngu, na.rm = TRUE)
cat("Mean-2021:", mean2021)

## Tính độ lệch chuẩn
sd2022 <- sd(loc_AV_2022$ngoai_ngu, na.rm = TRUE)
cat("Độ lệch chuẩn - 2022:", sd2022)

sd2021 <- sd(loc_AV_2021$ngoai_ngu, na.rm = TRUE)
cat("Độ lệch chuẩn - 2021:", sd2021)


## phân vùng 8 ký tự số báo danh
convert_sbd <- function(sbd) {
  sbd_str <- as.character(sbd)
  sbd_padded <- str_pad(sbd_str, width = 8, side = "left", pad = "0")
  return(sbd_padded)
}

# năm 2022
sbd_2022 <- data_2022$sbd
sbd_2022_padded <- convert_sbd(sbd_2022)
data_2022_sbd <- data_2022 %>% mutate(sbd_2022 = sbd_2022_padded)

#năm 2021
sbd_2021 <- data_2021$sbd
sbd_2021_padded <- convert_sbd(sbd_2021)
data_2021_sbd <- data_2021 %>% mutate(sbd_2021 = sbd_2021_padded)


## thêm data về tỉnh/thành phố
{
# Thay thế đoạn mã đọc dữ liệu địa lý
ma_tinh_thanh <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "21",
                   "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
                   "32", "33", "34", "35", "36", "37", "38", "39", "40", "41",
                   "42", "43", "44", "45", "46", "47", "48", "49", "50", "51",
                   "52", "53", "54", "55", "56", "57", "58", "59", "60", "61",
                   "62", "63", "64")

# Tạo tên tỉnh/thành phố tương ứng với mã tỉnh/thành phố
ten_tinh_thanh <- c("THÀNH PHỐ HÀ NỘI", "THÀNH PHỐ HỒ CHÍ MINH", "THÀNH PHỐ HẢI PHÒNG",
                    "THÀNH PHỐ ĐÀ NẴNG", "TỈNH HÀ GIANG", "TỈNH CAO BẰNG", "TỈNH LAI CHÂU",
                    "TỈNH LÀO CAI", "TỈNH TUYÊN QUANG", "TỈNH LẠNG SƠN", "TỈNH BẮC KẠN",
                    "TỈNH THÁI NGUYÊN", "TỈNH YÊN BÁI", "TỈNH SƠN LA", "TỈNH PHÚ THỌ",
                    "TỈNH VĨNH PHÚC", "TỈNH QUẢNG NINH", "TỈNH BẮC GIANG", "TỈNH BẮC NINH",
                    "TỈNH HẢI DƯƠNG", "TỈNH HƯNG YÊN", "TỈNH HÒA BÌNH", "TỈNH HÀ NAM",
                    "TỈNH NAM ĐỊNH", "TỈNH THÁI BÌNH", "TỈNH NINH BÌNH", "TỈNH THANH HÓA",
                    "TỈNH NGHỆ AN", "TỈNH HÀ TĨNH", "TỈNH QUẢNG BÌNH", "TỈNH QUẢNG TRỊ",
                    "TỈNH THỪA THIÊN - HUẾ", "TỈNH QUẢNG NAM", "TỈNH QUẢNG NGÃI", "TỈNH KON TUM",
                    "TỈNH BÌNH ĐỊNH", "TỈNH GIA LAI", "TỈNH PHÚ YÊN", "TỈNH ĐẮK LẮK", "TỈNH KHÁNH HÒA",
                    "TỈNH LÂM ĐỒNG", "TỈNH BÌNH PHƯỚC", "TỈNH BÌNH DƯƠNG", "TỈNH NINH THUẬN",
                    "TỈNH TÂY NINH", "TỈNH BÌNH THUẬN", "TỈNH ĐỒNG NAI", "TỈNH LONG AN",
                    "TỈNH ĐỒNG THÁP", "TỈNH AN GIANG", "TỈNH BÀ RỊA – VŨNG TÀU", "TỈNH TIỀN GIANG",
                    "TỈNH KIÊN GIANG", "THÀNH PHỐ CẦN THƠ", "TỈNH BẾN TRE", "TỈNH VĨNH LONG",
                    "TỈNH TRÀ VINH", "TỈNH SÓC TRĂNG", "TỈNH BẠC LIÊU", "TỈNH CÀ MAU",
                    "TỈNH ĐIỆN BIÊN", "TỈNH ĐĂK NÔNG", "TỈNH HẬU GIANG")

# Tạo bảng dữ liệu từ mã tỉnh/thành và tên tỉnh/thành
tinh_thanh_table <- data.frame(
  Ma_Tinh_Thanh = ma_tinh_thanh,
  Ten_Tinh_Thanh = ten_tinh_thanh
)
print(tinh_thanh_table)
}

# Chuyển đổi dữ liệu thành dạng dài
data_long_2022 <- gather(data_2022, key = "mon_hoc", value = "diem", -sbd)
# Tạo biểu đồ histogram tổng quan(đã phân)
ggplot(data_long_2022, aes(x = diem, fill = mon_hoc)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(x = "Điểm", y = "Số lượng") +
  ggtitle("Biểu đồ histogram tổng quan điểm thi THPT Quốc gia 2022") +
  scale_fill_discrete(name = "Môn học") +
  scale_y_continuous(labels = comma)+
  facet_grid(. ~ mon_hoc, scales = "free") +
  theme(legend.position = "right")

data_long_2021 <- gather(data_2021, key = "mon_hoc", value = "diem", -sbd)
data_long_2021
# Tạo biểu đồ histogram tổng quan(đã phân)
ggplot(data_long_2021, aes(x = diem, fill = mon_hoc)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(x = "Điểm", y = "Số lượng") +
  ggtitle("Biểu đồ histogram tổng quan điểm thi THPT Quốc gia 2021") +
  scale_fill_discrete(name = "Môn học") +
  scale_y_continuous(labels = comma)+
  facet_grid(. ~ mon_hoc, scales = "free") +
  theme(legend.position = "right")



# biểu diễn thang điểm 0 đến 10
a1 <- data_2022 %>%
  filter(ngoai_ngu >= 0 & ngoai_ngu <= 10 & !is.na(ngoai_ngu)) %>%
  ggplot(aes(ngoai_ngu)) +
  ggtitle("Phổ điểm môn ngoại ngữ năm 2022") +
  labs(x = "Điểm", y = "Số lượng học sinh") +
  geom_bar(col="yellow", fill="blue",na.rm = TRUE) +
  scale_x_continuous(breaks = seq(0, 10, by = 0.2), limits = c(0, 10.2)) +
  scale_y_continuous(labels = comma) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust=0, size=3, angle = 90, hjust = 0,col="black")
a1

a2 <- data_2021 %>%
  filter(ngoai_ngu >= 0 & ngoai_ngu <= 10 & !is.na(ngoai_ngu)) %>%
  ggplot(aes(ngoai_ngu)) +
  ggtitle("Phổ điểm môn ngoại ngữ năm 2021") +
  labs(x = "Điểm", y = "Số lượng học sinh") +
  geom_bar(col="brown", fill="lightgreen",na.rm = TRUE) +
  scale_x_continuous(breaks = seq(0, 10, by = 0.2), limits = c(0, 10.2)) +
  scale_y_continuous(labels = comma) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust=0, size=3, angle = 90, hjust = 0,col="black")
a2

#Biểu diễn 2 hình theo cột 
grid.arrange(a1, a2, ncol = 2)
# biểu diễn 2 hình theo hàng
plot_grid(a1, a2, ncol = 1, align = "v")

# Nhận xét biểu đồ:
# Cột có số điểm từ khoảng 3 đến 5 cho thấy số học sinh làm bài điểm trung bình chiếm rất nhiều và cao nhất là 4 điểm
# trong đó các thí sinh 10 điểm chiếm khá ít so cột 9 điểm, bên cạnh đó thì cũng có một phần nhỏ các thí sinh rơi vào điểm liệt
# Kết luận: Biểu đồ có xu hướng giảm dần từ cột điểm 4.0 nhưng bên cạnh các thang điểm 7-9 cũng khá cao 


data_liet_2022 <- data_2022 %>%
  filter(ngoai_ngu >= 0 & ngoai_ngu <= 1 & !is.na(data_2022$ngoai_ngu)) %>%
  group_by(ngoai_ngu) %>%
  summarise(SoLuong = n())

# Vẽ biểu đồ bar 2022
c1 = ggplot(data_liet_2022, aes(x = factor(ngoai_ngu), y = SoLuong)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Điểm Liệt", y = "Số lượng") +
  geom_text(aes(label = SoLuong), vjust = -0.5) +
  ggtitle("Số học sinh có điểm liệt năm 2022") +
  theme_minimal()


data_liet_2021 <- data_2021 %>%
  filter(ngoai_ngu >= 0 & ngoai_ngu < 1.2 & !is.na(data_2021$ngoai_ngu)) %>%
  group_by(ngoai_ngu) %>%
  summarise(SoLuong = n())

# Vẽ biểu đồ bar 2021
c2 = ggplot(data_liet_2021, aes(x = factor(ngoai_ngu), y = SoLuong)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(x = "Điểm Liệt", y = "Số lượng") +
  geom_text(aes(label = SoLuong), vjust = -0.5) +
  ggtitle("Số học sinh có điểm liệt năm 2021") +
  theme_minimal()

#Biểu diễn 2 hình trên 1 biểu đồ
plot_grid(c1, c2, ncol = 2, align = "s")


##########

###tổng các học sinh có điểm 10 năm 2022
tong_hs_10_diem <- sum(data_2022$ngoai_ngu == 10 & !is.na(anhvan_2022))
tong_hs_10_diem

tong_hs_9_diem <- sum(data$ngoai_ngu == 9 & !is.na(anhvan_2022))
tong_hs_9_diem

#số lượng học sinh 9 và 10 điểm năm 2022
hs_10_9_diem <- data %>%
  filter(!is.na(anhvan_2022) & (ngoai_ngu == 9 | ngoai_ngu == 10)) %>%
  ggplot(aes(x = factor(ngoai_ngu))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black") +
  labs(x = "Điểm", y = "Số lượng") +
  ggtitle("Số lượng thí sinh có điểm 9 và 10 môn Anh") +
  scale_x_discrete(labels = c("9", "10")) +
  theme_minimal()
hs_10_9_diem

###tổng các học sinh có điểm 10 năm 2021
tong_hs_10_diem_2021 <- sum(data_2021$ngoai_ngu == 10 & !is.na(anhvan_2021))
tong_hs_10_diem_2021
#Tổng học sinh bị 9 điểm năm 2021
tong_hs_9_diem_2021 <- sum(data_2021$ngoai_ngu == 9 & !is.na(anhvan_2021))
tong_hs_9_diem_2021


#số lượng học sinh 9 và 10 điểm năm 2021
hs_10_9_diem_2021 <- data_2021 %>%
  filter(!is.na(anhvan_2021) & (ngoai_ngu == 9 | ngoai_ngu == 10)) %>%
  ggplot(aes(x = factor(ngoai_ngu))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black") +
  labs(x = "Điểm", y = "Số lượng") +
  ggtitle("Số lượng thí sinh có điểm 9 và 10 môn Anh năm 2021") +
  scale_x_discrete(labels = c("9", "10")) +
  theme_minimal()
hs_10_9_diem_2021




# Create a combined dataset with matching column names and labels
combined_data <- rbind(
  data.frame(Year = "2022", Diem = "10", Count = nrow(data %>% filter(!is.na(anhvan_2022) & ngoai_ngu == 10))),
  data.frame(Year = "2022", Diem = "9", Count = nrow(data %>% filter(!is.na(anhvan_2022) & ngoai_ngu >= 9 & ngoai_ngu < 10))),
  data.frame(Year = "2021", Diem = "10", Count = nrow(data_2021 %>% filter(!is.na(anhvan_2021) & ngoai_ngu == 10))),
  data.frame(Year = "2021", Diem = "9", Count = nrow(data_2021 %>% filter(!is.na(anhvan_2021) & ngoai_ngu >= 9 & ngoai_ngu < 10)))
)

  # Define the order of the factors
  combined_data$Diem <- factor(combined_data$Diem, levels = c("9", "10"))
  
  # Plot the combined data with overlaid bar charts
  ggplot(combined_data, aes(x = Year, y = Count, fill = Diem)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
    labs(x = "Năm", y = "Số lượng", title = "Số lượng thí sinh có điểm 9 và 10 môn Anh năm 2021 và 2022") +
    scale_fill_manual(values = c("9" = "steelblue", "10" = "red")) +
    theme_minimal()


##################################################

## Tính phần trăm thí sinh đạt điểm 10 môn ngoại ngữ
num_diem_10 <- sum(anhvan == 10, na.rm = TRUE)
percent_diem_10 <- (num_diem_10 / length(anhvan)) * 100
cat("Phần trăm thí sinh đạt điểm 10 môn ngoại ngữ:", percent_diem_10, "%\n")

## Tính phần trăm thí sinh đạt điểm từ 8 trở lên môn ngoại ngữ
num_diem_8_above <- sum(anhvan >= 8, na.rm = TRUE)
percent_diem_8_above <- (num_diem_8_above / length(anhvan)) * 100
cat("Phần trăm thí sinh đạt điểm từ 8 trở lên môn Văn:", percent_diem_8_above, "%\n")

## Tính phần trăm thí sinh đạt điểm dưới 5 môn ngoại
num_diem_below_5 <- sum(anhvan <= 5, na.rm = TRUE)
percent_diem_below_5 <- (num_diem_below_5 / length(anhvan)) * 100
cat("Phần trăm thí sinh đạt điểm dưới 5 môn ngoại ngữ:", percent_diem_below_5, "%\n")
## tính phần trăm thí sinh đạt điểm từ 5-7 điểm môn ngoại ngữ
percent_diem_5_to_7 <- 100 - percent_diem_10 - percent_diem_8_above - percent_diem_below_5

# Tạo data frame chứa thông tin về các phần trăm
data_ps <- data.frame(
  LoaiDiem = c("Điểm 10", "Điểm từ 8 trở lên", "Điểm dưới 5",  "Điểm từ 5 đến 7"),
  PhanTram = c(percent_diem_10, percent_diem_8_above, percent_diem_below_5, percent_diem_5_to_7)
)

# Vẽ biểu đồ cột
ggplot(data_ps, aes(x = reorder(LoaiDiem, -PhanTram), y = PhanTram)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Phân bố điểm môn ngoại ngữ theo thang điểm 10",
       x = "Loại điểm",
       y = "Phần trăm") +
  geom_text(aes(label = sprintf("%.3f%%", PhanTram)), vjust = -0.5) +
  theme_minimal()


##################################################

# Mã phân vùng của từng thí sinh năm 2022
ma_phan_vung <- substr(data_2022_sbd$sbd_2022, 1, 2)
ma_phan_vung
{
# Tạo data frame từ các vector trên
df <- data.frame(ma_phan_vung, data_2022_sbd$sbd_2022, anhvan_2022)
# Lọc ra vùng có thí sinh nhiều điểm 10 môn anh nhất
vung_max_diem_10_anh_2022 <- names(table(ma_phan_vung[anhvan_2022 == 10]))
# Tìm tên tỉnh/thành phố tương ứng với các vùng được lọc
ten_tinh_thanh_max_diem_10_anh_2022 <- ten_tinh_thanh[ma_tinh_thanh %in% vung_max_diem_10_anh_2022]
# In kết quả
print(paste("Vùng có thí sinh điểm 10 môn ngoại ngữ:", vung_max_diem_10_anh_2022))
print(paste("Tên tỉnh/thành phố:", ten_tinh_thanh_max_diem_10_anh_2022))
# Tính số lượng điểm 10 của các vùng cao nhất
so_luong_diem_10_2022 <- table(ma_phan_vung[anhvan_2022 == 10])[vung_max_diem_10_anh_2022]
# In số lượng điểm 10 của các vùng cao nhất
print(paste("Số lượng điểm 10 của các vùng cao nhất:"))
for (i in 1:length(vung_max_diem_10_anh_2022)) {
  print(paste("Vùng:", vung_max_diem_10_anh_2022[i],"Thuộc: ", ten_tinh_thanh_max_diem_10_anh_2022[i]))
  print(paste("Số lượng điểm 10:", so_luong_diem_10_2022[i]))
}
}
#biểu đồ số lượng và phần trăm điểm 10 của năm 2022
{
# Tạo data frame cho biểu đồ cột
df_plot_2022 <- data.frame(Vung = ten_tinh_thanh_max_diem_10_anh_2022, SoLuongDiem10_2022 = so_luong_diem_10_2022)

# Tạo biểu đồ cột
d1 = ggplot(df_plot_2022, aes(x = SoLuongDiem10_2022.Freq, y = reorder(Vung, SoLuongDiem10_2022.Freq), fill = Vung)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = SoLuongDiem10_2022.Freq), hjust = -0.2,size = 3.5, color = "black") +
  labs(title = "Số lượng điểm 10 môn ngoại ngữ theo tỉnh/thành phố năm 2022") +
  xlab("Số lượng điểm 10") +
  ylab("Tỉnh/Thành phố")
d1
# Tính phần trăm điểm 10 cho từng vùng
phan_tram_diem_10_2022 <- round((so_luong_diem_10_2022 / table(ma_phan_vung)[vung_max_diem_10_anh_2022]) * 100, 2)

# Tạo data frame cho biểu đồ cột phần trăm
df_plot_percent_2022 <- data.frame(Vung = ten_tinh_thanh_max_diem_10_anh_2022, PhanTramDiem10_2022 = phan_tram_diem_10_2022)

# Tạo biểu đồ cột phần trăm
d2 = ggplot(df_plot_percent_2022, aes(x = PhanTramDiem10_2022.Freq, y = reorder(Vung, PhanTramDiem10_2022.Freq), fill = Vung)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = paste0(PhanTramDiem10_2022.Freq, "%")), hjust = -0.2,size = 3.5, color = "black") +
  labs(title = "Phần trăm điểm 10 môn ngoại ngữ theo tỉnh/thành phố năm 2022") +
  xlab("Phần trăm điểm 10") +
  ylab("Tỉnh/Thành phố")  
plot_grid(d1, d2, ncol = 2, align = "s")

}


# Mã phân vùng của từng thí sinh năm 2021
ma_phan_vung_2021 <- substr(data_2021_sbd$sbd_2021, 1, 2)
{
# Tạo data frame từ các vector trên
df <- data.frame(ma_phan_vung_2021, data_2021_sbd$sbd_2021, anhvan_2021)
# Lọc ra vùng có thí sinh nhiều điểm 10 môn anh nhất
vung_max_diem_10_anh_2021 <- names(table(ma_phan_vung[anhvan_2021 == 10]))
# Tìm tên tỉnh/thành phố tương ứng với các vùng được lọc
ten_tinh_thanh_max_diem_10_anh_2021 <- ten_tinh_thanh[ma_tinh_thanh %in% vung_max_diem_10_anh_2021]
# In kết quả
print(paste("Vùng có thí sinh điểm 10 môn ngoại ngữ:", vung_max_diem_10_anh_2021))
print(paste("Tên tỉnh/thành phố:", ten_tinh_thanh_max_diem_10_anh_2021))
# Tính số lượng điểm 10 của các vùng cao nhất
so_luong_diem_10_2021 <- table(ma_phan_vung[anhvan_2021 == 10])[vung_max_diem_10_anh_2021]

# In số lượng điểm 10 của các vùng cao nhất
print(paste("Số lượng điểm 10 của các vùng cao nhất:"))
for (i in 1:length(vung_max_diem_10_anh_2021)) {
  print(paste("Vùng:", vung_max_diem_10_anh_2021[i],"Thuộc: ", ten_tinh_thanh_max_diem_10_anh_2021[i]))
  print(paste("Số lượng điểm 10:", so_luong_diem_10_2021[i]))
}
}
# Biểu đồ số lượng và phần trăm điềm 10 của năm 2021
{
# Tạo data frame cho biểu đồ cột
df_plot_2021 <- data.frame(Vung_2021 = ten_tinh_thanh_max_diem_10_anh_2021, SoLuongDiem10_2021 = so_luong_diem_10_2021)
# Tạo biểu đồ cột
e1 = ggplot(df_plot_2021,aes(x = SoLuongDiem10_2021.Freq, y = reorder(Vung_2021, SoLuongDiem10_2021.Freq), fill = Vung_2021)) +
  geom_col(fill = "yellow") +
  geom_text(aes(label = SoLuongDiem10_2021.Freq), hjust = -0.2,size = 3.5, color = "black") +
  labs(title = "Số lượng điểm 10 môn ngoại ngữ theo tỉnh/thành phố năm 2021") +
  xlab("Số lượng điểm 10") +
  ylab("Tỉnh/Thành phố")

# Tính phần trăm điểm 10 cho từng vùng
phan_tram_diem_10_2021 <- round((so_luong_diem_10_2021 / table(ma_phan_vung)[vung_max_diem_10_anh_2021]) * 100,2)

# Tạo data frame cho biểu đồ cột phần trăm
df_plot_percent_2021 <- data.frame(Vung_2021 = ten_tinh_thanh_max_diem_10_anh_2021, PhanTramDiem10_2021 = phan_tram_diem_10_2021)

table(ma_phan_vung)[vung_max_diem_10_anh_2021]
# Tạo biểu đồ cột phần trăm
e2 = ggplot(df_plot_percent_2021, aes(x = PhanTramDiem10_2021.Freq, y = reorder(Vung_2021, PhanTramDiem10_2021.Freq), fill = Vung_2021)) +
  geom_col(fill = "yellow") +
  geom_text(aes(label = paste0(PhanTramDiem10_2021.Freq, "%")), hjust = -0.2,size = 3.5, color = "black") +
  labs(title = "Phần trăm điểm 10 môn ngoại ngữ theo tỉnh/thành phố năm 2021") +
  xlab("Phần trăm điểm 10") +
  ylab("Tỉnh/Thành phố")

plot_grid(e1, e2, ncol = 2, align = "s")
}

# biểu diễn số lượng điểm 10 giữa năm 2022 và 2021
#điểm 10 năm 2022: 118 điểm 10 năm 2021: 1178
plot_grid(d1, e1, ncol = 2, align = "s", rel_widths = c(1,1)) +
  theme(plot.margin = margin(t = -1, unit = "mm"))

# biểu diễn phần trăm điểm 10 giữa năm 2022 và 2021
# Phần trăm điểm 10 2022: 0.14% và phần trăm điểm 10 2021: 4.32
plot_grid(d2, e2, ncol = 2, align = "s", rel_widths = c(1,1)) +
  theme(plot.margin = margin(t = -1, unit = "mm"))


####################################################

# Mã phân vùng của từng thí sinh
ma_phan_vung_2022 <- substr(data_2022_sbd$sbd_2022, 1, 2)
# Tạo data frame từ các vector trên
df_2022 <- data.frame(ma_phan_vung_2022, data_2022_sbd$sbd_2022, anhvan_2022)
{
###Lọc vùng
# Lọc ra vùng có thí sinh điểm kém, trung bình 1-6.5 môn anh năm 2022
vung_1_6.5_anh <- names(table(ma_phan_vung_2022[anhvan_2022 > 1 & anhvan_2022 <6.5 ]))
vung_1_6.5_anh
# Lọc ra vùng có thí sinh điểm khá 6.5- 7.9 môn anh năm 2022
vung_6.5_8_anh <- names(table(ma_phan_vung_2022[anhvan_2022 >= 6.5 & anhvan_2022 < 8 ]))
vung_6.5_8_anh
# Lọc ra vùng có thí sinh điểm giỏi 8- 10 môn anh năm 2022
vung_8_10_anh <- names(table(ma_phan_vung_2022[anhvan_2022 >= 8 & anhvan_2022 <= 10 ]))
vung_8_10_anh

# Tìm tên tỉnh/thành phố tương ứng với các vùng được lọc
ten_tinh_thanh_1_6.5_anh <- ten_tinh_thanh[ma_tinh_thanh %in% vung_1_6.5_anh]
ten_tinh_thanh_6.5_8_anh <- ten_tinh_thanh[ma_tinh_thanh %in% vung_6.5_8_anh]
ten_tinh_thanh_8_10_anh <- ten_tinh_thanh[ma_tinh_thanh %in% vung_8_10_anh]
# In kết quả (chạy thử)
print(paste("Vùng có thí sinh điểm kém trung bình 1_ đến 6.5 điểm ngoại ngữ:", vung_1_6.5_anh))
print(paste("Tên tỉnh/thành phố:", ten_tinh_thanh_1_6.5_anh))

# Tính số lượng điểm 1-6.5 của các vùng cao nhất
so_luong_diem_1_6.5 <- table(ma_phan_vung_2022[anhvan_2022 > 1 & anhvan_2022 < 6.5 ])[vung_1_6.5_anh]
sort(so_luong_diem_1_6.5)
sum(so_luong_diem_1_6.5)
# Tương tự như trên tính lần 6.5 đến 7.9 
so_luong_diem_6.5_8 <- table(ma_phan_vung_2022[anhvan_2022 >= 6.5 & anhvan_2022 < 8 ])[vung_6.5_8_anh]
# 8 đến 10
so_luong_diem_8_10 <- table(ma_phan_vung_2022[anhvan_2022 >= 8 & anhvan_2022 <= 10 ])[vung_8_10_anh]

# Tổng số lượng thí sinh trong mỗi phân vùng
tong_thi_sinh_vung <- table(ma_phan_vung_2022)

# Tính phần trăm số lượng điểm kém trung bình
phan_tram_kem_trung_binh <- round((so_luong_diem_1_6.5 / tong_thi_sinh_vung[vung_1_6.5_anh]) * 100, 2)

# Tính phần trăm số lượng điểm khá
phan_tram_kha <- round((so_luong_diem_6.5_8 / tong_thi_sinh_vung[vung_6.5_8_anh]) * 100, 2)

# Tính phần trăm số lượng điểm giỏi
phan_tram_gioi <- round((so_luong_diem_8_10 / tong_thi_sinh_vung[vung_8_10_anh]) * 100, 2)

# In kết quả
print(paste("Phần trăm số lượng điểm kém trung bình:", phan_tram_kem_trung_binh))
print(paste("Phần trăm số lượng điểm khá:", phan_tram_kha))
print(paste("Phần trăm số lượng điểm giỏi:", phan_tram_gioi))
}

# Tạo data frame chứa kết quả phần trăm điểm kém trung bình, khá, giỏi của các vùng
df_percent <- data.frame(
  Vung = c(ten_tinh_thanh_1_6.5_anh, ten_tinh_thanh_6.5_8_anh, ten_tinh_thanh_8_10_anh),
  PhanTramKemTrungBinh = c(phan_tram_kem_trung_binh, rep(0, length(ten_tinh_thanh_6.5_8_anh)), rep(0, length(ten_tinh_thanh_8_10_anh))),
  PhanTramKha = c(rep(0, length(ten_tinh_thanh_1_6.5_anh)), phan_tram_kha, rep(0, length(ten_tinh_thanh_8_10_anh))),
  PhanTramGioi = c(rep(0, length(ten_tinh_thanh_1_6.5_anh)), rep(0, length(ten_tinh_thanh_6.5_8_anh)), phan_tram_gioi)
)

# Tạo data frame chứa kết quả phần trăm điểm kém trung bình của các vùng
{data_histogram <- data.frame(Vung_kem_trungbinh = c(ten_tinh_thanh_1_6.5_anh),
                             PhanTramDiem = c(phan_tram_kem_trung_binh))

# Sắp xếp data frame theo phần trăm giảm dần
data_histogram_sorted <- data_histogram %>%
  arrange(desc(PhanTramDiem))

# Vẽ biểu đồ histogram
ggplot(data_histogram_sorted, aes(x = PhanTramDiem, y = reorder(Vung_kem_trungbinh, PhanTramDiem), fill = Vung_kem_trungbinh)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Phần trăm điểm", y = "Tỉnh/Thành phố") +
  ggtitle("Biểu đồ histogram phần trăm điểm kém- trung bình (1 - 6.5) theo vùng năm 2022") +
  geom_text(aes(label = paste0(PhanTramDiem, "%")), hjust = -0.2,size = 3.5, color = "black") +
  theme_minimal() +
  theme(axis.text.y = element_text(, hjust = 1))
}

# Tạo data frame chứa kết quả phần trăm điểm phần trăm điểm khá của các vùng
{data_histogram <- data.frame(
  Vung_kha = c(ten_tinh_thanh_6.5_8_anh),
  PhanTramDiem = c(phan_tram_kha))

# Sắp xếp data frame theo phần trăm giảm dần
data_histogram_sorted <- data_histogram %>%
  arrange(desc(PhanTramDiem))

# Vẽ biểu đồ histogram
ggplot(data_histogram_sorted, aes(x = PhanTramDiem, y = reorder(Vung_kha, PhanTramDiem), fill = Vung_kha)) +
  geom_bar(stat = "identity",col = "brown", fill = "lightgreen") +
  geom_text(aes(label = paste0(PhanTramDiem, "%")), hjust = -0.2, size = 3.5, color = "black") +
  labs(x = "Phần trăm điểm", y = "Tỉnh/Thành phố") +
  ggtitle("Biểu đồ histogram phần trăm điểm khá (6.5 - 8) theo vùng năm 2022") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))
}

# Tạo data frame chứa kết quả phần trăm điểm phần trăm điểm giỏi của các vùng
{data_histogram <- data.frame(
Vung_gioi = c(ten_tinh_thanh_8_10_anh),
PhanTramDiem = c(phan_tram_gioi))

# Sắp xếp data frame theo phần trăm giảm dần
data_histogram_sorted <- data_histogram %>%
  arrange(desc(PhanTramDiem))

# Vẽ biểu đồ histogram
ggplot(data_histogram_sorted, aes(x = PhanTramDiem, y = reorder(Vung_gioi, PhanTramDiem), fill = Vung_gioi)) +
  geom_bar(stat = "identity",col = "blue", fill = "grey") +
  geom_text(aes(label = paste0(PhanTramDiem, "%")), hjust = -0.2, size = 3.5, color = "black") +
  labs(x = "Phần trăm điểm", y = "Tỉnh/Thành phố") +
  ggtitle("Biểu đồ histogram phần trăm điểm giỏi (8-10) theo vùng năm 2022") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))
}




