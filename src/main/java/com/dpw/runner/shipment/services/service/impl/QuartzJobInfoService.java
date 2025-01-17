package com.dpw.runner.shipment.services.service.impl;

import com.dpw.api.quartz.job.SimpleJob;
import com.dpw.api.quartz.service.QuartzJobService;
import com.dpw.messaging.api.response.QuartzJobResponse;
import com.dpw.runner.shipment.services.commons.constants.QuartzJobInfoConstants;
import com.dpw.runner.shipment.services.dao.impl.QuartzJobInfoDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse.FileTransferConfigurations;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.enums.PrePostTrigger;
import com.dpw.runner.shipment.services.entity.enums.TimeUnit;
import com.dpw.runner.shipment.services.service.interfaces.IQuartzJobInfoService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.entity.enums.FileTransferCriteriaFields;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

@Slf4j
@Service
public class QuartzJobInfoService implements IQuartzJobInfoService {

    private final QuartzJobService quartzJobService;
    private final IQuartzJobInfoDao quartzJobInfoDao;
    private final CommonUtils commonUtils;

    @Autowired
    QuartzJobInfoService(QuartzJobService quartzJobService, QuartzJobInfoDao quartzJobInfoDao, CommonUtils commonUtils) {
        this.quartzJobService = quartzJobService;
        this.quartzJobInfoDao = quartzJobInfoDao;
        this.commonUtils=commonUtils;
    }

    public QuartzJobResponse createSimpleJob(QuartzJobInfo jobInfo) {
        Date startDate = Date.from(jobInfo.getStartTime().atZone(ZoneId.systemDefault()).toInstant());
        long startTimeForQuartz = System.currentTimeMillis();
        QuartzJobResponse quartzJobResponse = quartzJobService.scheduleOneTimeJob(jobInfo.getId().toString(), SimpleJob.class, startDate);
        log.info(QuartzJobInfoConstants.TIME_TAKE_TO_SCHEDULE_JOB, jobInfo.getId().toString(),
                System.currentTimeMillis()-startTimeForQuartz);
        return quartzJobResponse;
    }

    public QuartzJobResponse updateSimpleJob(QuartzJobInfo jobInfo) {
        Date startDate = Date.from(jobInfo.getStartTime().atZone(ZoneId.systemDefault()).toInstant());
        long startTimeForQuartz = System.currentTimeMillis();
        QuartzJobResponse quartzJobResponse = quartzJobService.updateOneTimeJob(jobInfo.getId().toString(), startDate);
        log.info(QuartzJobInfoConstants.TIME_TAKE_TO_SCHEDULE_JOB, jobInfo.getId().toString(),
                System.currentTimeMillis()-startTimeForQuartz);
        return quartzJobResponse;
    }

    @Transactional
    public QuartzJobResponse deleteJobById(Long jobId) {
        long startTimeForQuartz = System.currentTimeMillis();
        quartzJobInfoDao.deleteById(jobId);
        QuartzJobResponse quartzJobResponse = quartzJobService.deleteJob(jobId.toString());
        log.info(QuartzJobInfoConstants.TIME_TAKE_TO_SCHEDULE_JOB, jobId,
                System.currentTimeMillis()-startTimeForQuartz);
        return quartzJobResponse;
    }


    @Override
    public LocalDateTime getQuartzJobTime(LocalDateTime eta, LocalDateTime etd, LocalDateTime ata, LocalDateTime atd) {
        List<FileTransferConfigurations> fileTransferConfigurations = getActiveFileTransferConfigurations();
        if (ObjectUtils.isEmpty(fileTransferConfigurations)) {
            return null;
        }

        HashMap<String, LocalDateTime> finalMap = new HashMap<>();
        LocalDateTime jobDateTime = null;

        for (var fileTransferConfiguration : fileTransferConfigurations) {
            LocalDateTime newTime = calculateJobTime(fileTransferConfiguration, eta, etd, ata, atd, finalMap);
            if (newTime != null && (jobDateTime == null || newTime.isBefore(jobDateTime))) {
                    jobDateTime = newTime;
                }
        }

        return jobDateTime;
    }

    public List<FileTransferConfigurations> getActiveFileTransferConfigurations() {
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        List<FileTransferConfigurations> configurations = tenantSettingsResponse.getFileTransferConfigurations();
        if (ObjectUtils.isEmpty(configurations)) {
            return Collections.emptyList();
        }
        configurations.removeIf(config -> config.getIsActive() == 0);
        return configurations;
    }

    private LocalDateTime calculateJobTime(FileTransferConfigurations fileTransferConfiguration, LocalDateTime eta,
                                           LocalDateTime etd, LocalDateTime ata, LocalDateTime atd,
                                           HashMap<String, LocalDateTime> finalMap) {
        String transportMode = fileTransferConfiguration.getTransportMode();
        if (finalMap.containsKey(transportMode)) {
            return null;
        }

        LocalDateTime baseDateTime = getBaseDateTime(fileTransferConfiguration.getCriteriaField(), eta, etd, ata, atd);
        if (baseDateTime == null) {
            return null;
        }

        int intervalTime = fileTransferConfiguration.getIntervalTime();
        String trigger = PrePostTrigger.getDescriptionById(fileTransferConfiguration.getTriggerType());
        String intervalUnit = TimeUnit.getDescriptionById(fileTransferConfiguration.getIntervalTimeUnit());

        ChronoUnit unit = getChronoUnit(intervalUnit);
        LocalDateTime newTime = "+".equals(trigger)
                ? baseDateTime.plus(intervalTime, unit)
                : baseDateTime.minus(intervalTime, unit);

        finalMap.put(transportMode, newTime);
        return newTime;
    }

    private LocalDateTime getBaseDateTime(int criteria, LocalDateTime eta, LocalDateTime etd, LocalDateTime ata, LocalDateTime atd) {
        if (criteria == FileTransferCriteriaFields.ETA.getId() && eta != null) {
            return eta;
        } else if (criteria == FileTransferCriteriaFields.ETD.getId() && etd != null) {
            return etd;
        } else if (criteria == FileTransferCriteriaFields.ATA.getId() && ata != null) {
            return ata;
        } else if (criteria == FileTransferCriteriaFields.ATD.getId() && atd != null) {
            return atd;
        }
        return null;
    }

    private ChronoUnit getChronoUnit(String intervalUnit) {
        return "hour".equalsIgnoreCase(intervalUnit) ? ChronoUnit.HOURS : ChronoUnit.DAYS;
    }

    public boolean isJobWithNamePresent(String jobName){
        return quartzJobService.isJobWithNamePresent(jobName);
    }

}
