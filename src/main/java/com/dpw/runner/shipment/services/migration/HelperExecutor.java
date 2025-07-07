package com.dpw.runner.shipment.services.migration;

import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.CompletableFuture;

@Service
@EnableAsync
@Slf4j
public class HelperExecutor {

    @Transactional
    public <V> V runInTrx(IRun<V> runnable) {
        return runnable.run();
    }

    @Async
    public <V> CompletableFuture<V> runInAsync(IRun<V> runnable) {
        return CompletableFuture.completedFuture(runnable.run());
    }

}
