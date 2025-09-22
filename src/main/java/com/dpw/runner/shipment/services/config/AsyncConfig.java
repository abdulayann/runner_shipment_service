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
@Slf4j
@Generated
public class AsyncConfig implements AsyncConfigurer {

    // Used for Async
    @Bean(name = "asyncExecutor")
    public ThreadPoolTaskExecutor taskExecutor() {
        return createBackupRestoreMigrationExecutor("MyAsyncThread-", 20, 20);
    }

    @Bean(name = "asyncShipmentBackupHandlerExecutor")
    public ThreadPoolTaskExecutor backupShipmentHandlerExecutor() {
        return createExecutor("BackupShipmentHandlerAsyncThread-", 10, 10);
    }

    @Bean(name = "asyncRestoreHandlerExecutor")
    public ThreadPoolTaskExecutor rollbackTaskExecutor() {
        return createBackupRestoreMigrationExecutor("RestoreThread-", 5, 5);
    }

    @Bean(name = "asyncBackupHandlerExecutor")
    public ThreadPoolTaskExecutor backupHandlerExecutor() {
        return createBackupRestoreMigrationExecutor("BackupHandlerAsyncThread-", 5, 5);
    }


    @Bean(name = "asyncConsoleBackupHandlerExecutor")
    public ThreadPoolTaskExecutor asyncConsoleBackupHandlerExecutor() {
        return createExecutor("BackupConsoleHandlerAsyncThread-", 10, 10);
    }

    @Bean(name = "asyncBookingBackupHandlerExecutor")
    public ThreadPoolTaskExecutor backupBookingHandlerExecutor() {
        return createExecutor("BackupBookingHandlerAsyncThread-", 10, 10);
    }

    @Bean(name = "asyncNetworkTransferBackupHandlerExecutor")
    public ThreadPoolTaskExecutor backupNetworkTransferHandlerExecutor() {
        return createExecutor("BackupNetworkTransferHandlerAsyncThread-", 10, 10);
    }


    @Bean(name = "asyncExecutorForConsole")
    public ThreadPoolTaskExecutor asyncConsoleRestoreHandlerExecutor() {
        return createExecutor("RestoreConsoleHandlerAsyncThread-", 10, 20);
    }

    @Bean(name = "asyncExecutorForBooking")
    public ThreadPoolTaskExecutor asyncBookingRestoreHandlerExecutor() {
        return createExecutor("RestoreBookingHandlerAsyncThread-", 10, 20);
    }

    @Bean(name = "asyncExecutorForShipment")
    public ThreadPoolTaskExecutor asyncShipmentRestoreHandlerExecutor() {
        return createExecutor("RestoreShipmentHandlerAsyncThread-", 10, 20);
    }

    @Bean(name = "asyncExecutorForMigration3")
    public ThreadPoolTaskExecutor taskExecutorForMigration3() {
        return createBackupRestoreMigrationExecutor("MyMigrationAsyncThread-", 10, 10);
    }

    private ThreadPoolTaskExecutor createBackupRestoreMigrationExecutor(String threadNamePrefix, int corePoolSize, int maxPoolSize) {

        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setRejectedExecutionHandler((r, executor1) -> log.warn(SyncingConstants.TASK_REJECTION_WARNING_MSG));
        executor.initialize();
        return executor;
    }

    private ThreadPoolTaskExecutor createExecutor(String threadNamePrefix, int corePoolSize, int maxPoolSize) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setQueueCapacity(100);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setAllowCoreThreadTimeOut(true);
        executor.setKeepAliveSeconds(60);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(60);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        return executor;
    }


    // Used for Completable future
    @Bean
    @Primary
    public ExecutorService executorService() {
        int corePoolSize = 20; // Min threads
        int maximumPoolSize = 50; // Adjusted max pool size
        long keepAliveTime = 1800; // Keep alive time for idle threads
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
        int corePoolSize = 20; // Min threads
        int maximumPoolSize = 50; // Adjusted max pool size
        long keepAliveTime = 1800; // Keep alive time for idle threads
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
                log.error("Task interrupted while waiting for the queue space", e);
            }
        };
        return new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler);
    }

    @Bean
    public ExecutorService executorServiceRouting() {
        return commonExecutorService(5, 10, 100);
    }

    public ExecutorService commonExecutorService(int corePoolSize, int maximumPoolSize, int queueCapacity){
        long keepAliveTime = 1800; // Keep alive time for idle threads
        TimeUnit unit = TimeUnit.SECONDS;
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


    @Bean
    public ExecutorService executorServiceReport() {
        int corePool = 5; // Min threads
        int maxPool = 30; // Adjusted max pool size
        long aliveTime = 1800; // Keep alive time for idle threads
        TimeUnit unit = TimeUnit.SECONDS;
        int queueCapacity = 100; // Define queue capacity
        BlockingQueue<Runnable> workingQueue = new LinkedBlockingQueue<>(queueCapacity);
        RejectedExecutionHandler executionHandler = (r, executor) -> {
            try {
                // Set maximum wait time for the queue
                if (!workingQueue.offer(r, 5, TimeUnit.SECONDS)) {
                    log.warn("executorServiceRouting- Task rejected, thread pool is full and queue is also full");
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("executorServiceRouting- Task interrupted while waiting for queue space", e);
            }
        };
        return new ThreadPoolExecutor(corePool, maxPool, aliveTime, unit, workingQueue, executionHandler);
    }

    @Bean(name = "asyncHsCodeValidationExecutor")
    public ThreadPoolTaskExecutor hsCodeValidationExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(10);
        executor.setMaxPoolSize(10);
        executor.setQueueCapacity(100);
        executor.setThreadNamePrefix("HsCodeValidationExecutorAsyncThread-");
        executor.setAllowCoreThreadTimeOut(true);
        executor.setKeepAliveSeconds(60);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(60);
        executor.setRejectedExecutionHandler((r, executor1) -> log.warn(SyncingConstants.TASK_REJECTION_WARNING_MSG));
        executor.initialize();
        return executor;
    }
}
