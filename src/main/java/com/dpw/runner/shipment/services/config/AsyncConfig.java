package com.dpw.runner.shipment.services.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Configuration
@EnableAsync
@Slf4j
public class AsyncConfig implements AsyncConfigurer {

    @Bean(name = "asyncExecutor")
    public AsyncTaskExecutor getAsyncExecutor() {
        return new SimpleAsyncTaskExecutor();
    }

    // Used for Async
    @Bean
    public ThreadPoolTaskExecutor taskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(10);
        executor.setMaxPoolSize(10);
        executor.setThreadNamePrefix("MyAsyncThread-");
        executor.setRejectedExecutionHandler((r, executor1) -> log.warn("Task rejected, thread pool is full and queue is also full"));
        executor.initialize();
        return executor;
    }

    // Used for Completable future
    @Bean
    public ExecutorService executorService() {
        return Executors.newFixedThreadPool(10);
    }
}
