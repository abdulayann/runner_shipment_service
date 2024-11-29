package com.dpw.runner.shipment.services.service.impl;

import com.dpw.api.quartz.service.QuartzJobExecutorService;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entitytransfer.dto.request.SendShipmentRequest;
import com.dpw.runner.shipment.services.entitytransfer.service.interfaces.IEntityTransferService;
import lombok.extern.slf4j.Slf4j;
import org.quartz.JobExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;

@Slf4j
@Component
public class ShipmentJobExecutorService implements QuartzJobExecutorService {

    @Autowired
    private IEntityTransferService entityTransferService;

    @Override
    public void executeJob(JobExecutionContext jobExecutionContext) {
        String jobId = jobExecutionContext.getJobDetail().getKey().getName();
        log.info("Executing Job {}", jobId);
        try {
            // Perform task
        } catch (Exception ex) {
            log.error("Job Execution failed: " + ex.getMessage(), ex);
        }

    }
}
