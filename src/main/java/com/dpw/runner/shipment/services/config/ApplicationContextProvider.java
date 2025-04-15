package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Configuration;

@Configuration
@Generated
public class ApplicationContextProvider implements ApplicationContextAware {
    @Override
    public void setApplicationContext(@NotNull ApplicationContext applicationContext) throws BeansException {
        SpringContext.setApplicationContext(applicationContext);
    }
}
