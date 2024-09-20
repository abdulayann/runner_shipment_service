package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
@Generated
public class SpringContext {
    private static ApplicationContext context;

    public static void setApplicationContext(ApplicationContext applicationContext) {
        context = applicationContext;
    }

    public static <T> T getBean(Class<T> beanClass) {
        return context.getBean(beanClass);
    }
}
