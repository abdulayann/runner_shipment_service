//package com.dpw.runner.shipment.services.datasource;
//
//import org.springframework.beans.factory.annotation.Qualifier;
//import org.springframework.boot.autoconfigure.domain.EntityScan;
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.context.annotation.Primary;
//import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
//import org.springframework.orm.jpa.JpaTransactionManager;
//import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
//import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
//import org.springframework.transaction.PlatformTransactionManager;
//import org.springframework.transaction.annotation.EnableTransactionManagement;
//
//import javax.persistence.EntityManagerFactory;
//import javax.sql.DataSource;
//import java.util.HashMap;
//import java.util.Map;
//
//@Configuration
//@EnableTransactionManagement
//@EnableJpaRepositories(
//    basePackages = "com.dpw.runner.shipment.services.repository.interfaces",  // Change to your primary repository package
//    entityManagerFactoryRef = "ternaryEntityManagerFactory",
//    transactionManagerRef = "ternaryTransactionManager"
//)
////@EntityScan(basePackages = { "com.dpw.runner.shipment.services" })  // Change to your primary entity package
//public class TernaryDataSourceConfig {
//
//    @Bean(name = "ternaryDataSource")
//    @ConfigurationProperties(prefix = "spring.third-datasource")
//    public DataSource ternaryDataSource() {
//        return org.springframework.boot.jdbc.DataSourceBuilder.create().build();
//    }
//
//    @Bean(name = "ternaryEntityManagerFactory")
//    public LocalContainerEntityManagerFactoryBean ternaryEntityManagerFactory( EntityManagerFactoryBuilder builder,
//            @Qualifier("ternaryDataSource") DataSource dataSource) {
//
//        Map<String, Object> properties = new HashMap<>();
//        properties.put("hibernate.dialect", "org.hibernate.dialect.SQLServer2012Dialect");
//        properties.put("hibernate.hbm2ddl.auto", "none");
//        properties.put("hibernate.show-sql", "true");
//
//        return builder.dataSource(dataSource).properties(properties).packages("com.dpw.runner.shipment.services.entity")
//                .persistenceUnit("ternary")
//                .build();
//
//
////        LocalContainerEntityManagerFactoryBean entityManagerFactoryBean = new LocalContainerEntityManagerFactoryBean();
////        entityManagerFactoryBean.setDataSource(dataSource);
////        entityManagerFactoryBean.setPackagesToScan("com.dpw.runner.shipment.services");  // Set to your primary model package
////        entityManagerFactoryBean.setJpaVendorAdapter(new HibernateJpaVendorAdapter());
////        return entityManagerFactoryBean;
//    }
//
//    @Bean(name = "ternaryTransactionManager")
//    public PlatformTransactionManager ternaryTransactionManager(
//            @Qualifier("ternaryEntityManagerFactory") EntityManagerFactory ternaryEntityManagerFactory) {
//        return new JpaTransactionManager(ternaryEntityManagerFactory);
//    }
//}
