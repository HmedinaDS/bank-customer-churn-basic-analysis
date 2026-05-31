install.packages("tidyverse","janitor")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
initial_data<-read_csv("Bank Customer Churn Prediction.csv")
View(initial_data)

#Data observation
head(initial_data)
summary(initial_data)
glimpse(initial_data)
str(initial_data)

#lets see if we have duplicate data using the ID

initial_data %>%
  count(customer_id) %>%
  filter(n>1)

# CLEAN VALUES
#cleaning credit score

clean_cs<-initial_data %>%
  mutate(
    credit_score = case_when(
      credit_score<350~NA_real_,
      credit_score>850~NA_real_,
      TRUE~credit_score
    ),
    credit_score=replace_na(credit_score,mean(credit_score,na.rm=TRUE))
  )

summary(clean_cs$credit_score)

#now, we clean the countries
summary(clean_cs$country)

clean_countries<-clean_cs %>%
  mutate( country= str_to_lower(country))

summary(clean_countries$country)

# clean tenure, using mean, because the bank is only 10 years old.
summary(clean_countries$tenure)

ggplot(data=clean_countries,aes(x=tenure))+geom_boxplot()

clean_tn<-clean_countries%>%
  mutate(
    tenure=case_when(
      tenure>20~NA_real_,
      TRUE~tenure
    ),
    tenure=replace_na(tenure,mean(tenure,na.rm=TRUE))
  )
summary(clean_tn$tenure)

#now, we continue with the age and product number
summary(clean_tn$age)
clean_ap<-clean_tn %>%
  mutate(
    age=replace_na(age,mean(age,na.rm=TRUE)),
    products_number=replace_na(products_number,median(products_number,na.rm =TRUE))
    )

summary(clean_ap$age)
summary(clean_ap$products_number)

#finally, we clean the column "estimated salary" because we have
#value = 11.52

summary(clean_ap$estimated_salary)

ggplot(data=clean_ap,aes(x=estimated_salary))+
  geom_boxplot()

#we replace these value even if this does not affect the mean of the estimated salary

ds_final<-clean_ap%>%
  mutate(
    estimated_salary=case_when(
      estimated_salary<50~mean(estimated_salary),
    TRUE~estimated_salary
    )
  )

    summary(ds_final$estimated_salary)
  
    #now we are gonna graph some functions to make inferences
    
    #we want to see how many operations we have by country
    
    data_bc<-ds_final%>%
      group_by(country)
    
    ggplot(data=data_bc,aes(x=country,fill=country))+geom_bar(position="dodge")+
      theme_minimal()+
      labs(title="Data per country", subtitle="bank churn information",
                            x= "Country",y="Count")
    
    #now, we want to relate the churn by country
    
    data_cc<-ds_final %>%
      mutate(
        churn_label=factor(churn,levels=c(0,1),labels=c("Stayed","churned")),
                    country=as.factor(country)
      )
      
    ggplot(data=data_cc,aes(x=country,fill = churn_label))+ 
      geom_bar(position="dodge")+
      labs(title="Customer churn by country",x="Country",y="Number of clients",
           fill="status")+
      theme_minimal()+
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16), 
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.major.x = element_blank()
      )
    
  #we can see here that France and germany have the most highest rate of churn
    
    #2
    
    # impact of the age in the churn
    
    ggplot(data=data_cc, aes(x=churn_label,y=age,fill=churn_label))+
      geom_boxplot()+
      labs(title="Customer churn distribution by age",x="Churn",y="Age")+
      theme_minimal(base_size = 13)+
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16), 
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.major.x = element_blank()
        )
    
    #3
    #lets see if the number of products is related with the churn
    
    ggplot(data=data_cc,aes(x=products_number,fill=churn_label))+
      geom_bar()+
      labs(title="Customer churn by products number",
           x="Product number", y="Churn count")+
      theme_minimal(base_size=13)+
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16), 
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.major.x = element_blank()
      )
    
    #active member vs inactive member related to churn
  #prepare data
    
    newdata<-data_cc %>%
      group_by(active_member) %>%
      summarise(
        totalclientes=n(),
        tasachurn=mean(churn)
      ) %>%
      mutate(
        active_inac=factor(active_member,levels=c(0,1),labels=c("inactive","active")),
        active_member=as.factor(active_member)
      )
    
    ggplot(data = newdata, aes(x = active_inac, y = tasachurn, fill = active_inac)) +
      geom_col(alpha = 0.85, width = 0.5) +
      geom_text(
        aes(label = scales::percent(tasachurn, accuracy = 0.1)),
        vjust = -0.6, 
        fontface = "bold", 
        size = 5
      ) +
      scale_fill_manual(values = c("inactive" = "#e63946", "active" = "#457b9d")) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 0.40)) +
      labs(
        title = "Churn Rate by Member Activity",
        subtitle = "Inactive members are significantly more likely to leave the bank",
        x = "Activity Status",
        y = "Churn Rate (%)"
      ) +
      
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",          
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.major.x = element_blank() 
      )
  
    #Correlation matrix
   
    data_num <- ds_final %>%
      select(credit_score, age, tenure, balance, products_number, 
             credit_card, active_member, estimated_salary, churn)
    
    
    matriz_corr <- cor(data_num, use = "complete.obs")
    
    df_corr <- as.data.frame(matriz_corr) %>%
      mutate(Variable1 = rownames(.)) %>%
      pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlacion")
  
    ggplot(data = df_corr, aes(x = Variable1, y = Variable2, fill = Correlacion)) +
      geom_tile(color = "white") +
      
      
      scale_fill_gradient2(low = "#e63946", mid = "white", high = "#457b9d", 
                           midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      
      
      geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 3.5) +
      
      
      labs(
        title = "Correlation Matrix Heatmap",
        subtitle = "Analysis of numerical features and their relationship with churn",
        x = NULL, y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank()
      )
   
    
    write_csv(ds_final, "Bank_Customer_Churn_Clean.csv")
  