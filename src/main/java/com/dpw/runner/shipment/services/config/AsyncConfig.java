package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.*;

@Configuration
@EnableAsync
@Slf4j @Generated
public class AsyncConfig implements AsyncConfigurer {

    // Used for Async
    @Bean(name = "asyncExecutor")
    public ThreadPoolTaskExecutor taskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(20);
        executor.setMaxPoolSize(20);
        executor.setThreadNamePrefix("MyAsyncThread-");
        executor.setRejectedExecutionHandler((r, executor1) -> log.warn(SyncingConstants.TASK_REJECTION_WARNING_MSG));
        executor.initialize();
        return executor;
    }

    // Used for Completable future
    @Bean
    @Primary
    public ExecutorService executorService() {
        int corePoolSize = 50; // Min threads
        int maximumPoolSize = 50; // Adjusted max pool size
        long keepAliveTime = 60; // Keep alive time for idle threads
        TimeUnit unit = TimeUnit.SECONDS;
        int queueCapacity = 250; // Define queue capacity
        BlockingQueue<Runnable> workQueue = new LinkedBlockingQueue<>(queueCapacity);
        RejectedExecutionHandler handler = (r, executor) -> {
            try {
                // Set maximum wait time for the queue
                if (!workQueue.offer(r, 5, TimeUnit.SECONDS)) {
                    log.warn(SyncingConstants.TASK_REJECTION_WARNING_MSG);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("Task interrupted while waiting for queue space", e);
            }
        };
        return new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler);
    }

    @Bean
    public ExecutorService executorServiceMasterData() {
        int corePoolSize = 50; // Min threads
        int maximumPoolSize = 50; // Adjusted max pool size
        long keepAliveTime = 60; // Keep alive time for idle threads
        TimeUnit unit = TimeUnit.SECONDS;
        int queueCapacity = 250; // Define queue capacity
        BlockingQueue<Runnable> workQueue = new LinkedBlockingQueue<>(queueCapacity);
        RejectedExecutionHandler handler = (r, executor) -> {
            try {
                // Set maximum wait time for the queue
                if (!workQueue.offer(r, 5, TimeUnit.SECONDS)) {
                    log.warn(SyncingConstants.TASK_REJECTION_WARNING_MSG);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("Task interrupted while waiting for queue space", e);
            }
        };
        return new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler);
    }
    @Bean
    public ExecutorService executorServiceRouting() {
        int corePoolSize = 10; // Min threads
        int maximumPoolSize = 20; // Adjusted max pool size
        long keepAliveTime = 60; // Keep alive time for idle threads
        TimeUnit unit = TimeUnit.SECONDS;
        int queueCapacity = 100; // Define queue capacity
        BlockingQueue<Runnable> workQueue = new LinkedBlockingQueue<>(queueCapacity);
        RejectedExecutionHandler handler = (r, executor) -> {
            try {
                // Set maximum wait time for the queue
                if (!workQueue.offer(r, 5, TimeUnit.SECONDS)) {
                    log.warn("executorServiceRouting- Task rejected, thread pool is full and queue is also full");
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("executorServiceRouting- Task interrupted while waiting for queue space", e);
            }
        };
        return new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler);
    }


}
