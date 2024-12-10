package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.entity.CommonErrorLogs;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendConsoleValidationResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.response.SendShipmentValidationResponse;
import com.dpw.runner.shipment.services.repository.interfaces.ICommonErrorLogsRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.stream.Collectors;

@Repository
@Slf4j
public class CommonErrorLogsDao implements ICommonErrorLogsDao {
    private final ICommonErrorLogsRepository commonErrorLogsRepository;
    @Autowired
    public CommonErrorLogsDao(ICommonErrorLogsRepository commonErrorLogsRepository) {
        this.commonErrorLogsRepository = commonErrorLogsRepository;
    }

    @Override
    public CommonErrorLogs save(CommonErrorLogs commonErrorLogs) {
        return commonErrorLogsRepository.save(commonErrorLogs);
    }

    public void deleteAllById(List<Long> ids) {
        commonErrorLogsRepository.deleteAllById(ids);
    }

    @Override
    public Optional<CommonErrorLogs> findById(Long id) {
        return commonErrorLogsRepository.findById(id);
    }

    @Override
    public List<CommonErrorLogs> findByEntityIdAndEntityTypeAndErrorType(Long entityId, String entityType, CommonErrorType errorType) {
        return commonErrorLogsRepository.findByEntityIdAndEntityTypeAndErrorType(entityId, entityType, errorType.name());
    }

    @Override
    public void logConsoleAutomaticTransferErrors(SendConsoleValidationResponse sendConsoleValidationResponse, Long consoleId, List<Long> shipmentIds) {

        if(Boolean.TRUE.equals(sendConsoleValidationResponse.getIsError())) {
            // Console related Errors Handled
            var commonErrors = this.findByEntityIdAndEntityTypeAndErrorType(consoleId, Constants.CONSOLIDATION, CommonErrorType.AUTOMATIC_TRANSFER);
            CommonErrorLogs commonErrorLogs;
            if (CommonUtils.listIsNullOrEmpty(commonErrors)) {
                commonErrorLogs = CommonErrorLogs.builder()
                        .entityId(consoleId)
                        .entityType(Constants.CONSOLIDATION)
                        .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                        .errorMessage("Automatic file transfer has failed. " + sendConsoleValidationResponse.getConsoleErrorMessage())
                        .build();
            } else {
                commonErrorLogs = commonErrors.get(0);
                commonErrorLogs.setErrorMessage("Automatic file transfer has failed. " + sendConsoleValidationResponse.getConsoleErrorMessage());
            }

            // Logged console related error
            save(commonErrorLogs);

            // All shipment related Errors Handled
            List<Long> failedShipIds = sendConsoleValidationResponse.getShipmentIds();
            List<CommonErrorLogs> shipCommonErrorList = new ArrayList<>();
            if (CommonUtils.listIsNullOrEmpty(shipmentIds)) {
                var shipCommonErrors = commonErrorLogsRepository.findByEntityIdListAndEntityTypeAndErrorType(shipmentIds, Constants.SHIPMENT, CommonErrorType.AUTOMATIC_TRANSFER.name());
                Map<Long, CommonErrorLogs> shipCommonErrorsMap = shipCommonErrors.stream().collect(Collectors.toMap(CommonErrorLogs::getEntityId, x -> x));
                for (Long shipId : failedShipIds) {
                    if (shipCommonErrorsMap.containsKey(shipId)) {
                        shipCommonErrorsMap.get(shipId).setErrorMessage(sendConsoleValidationResponse.getShipmentErrorMessage());
                        shipCommonErrorList.add(shipCommonErrorsMap.get(shipId));
                        shipCommonErrorsMap.remove(shipId);
                    } else {
                        shipCommonErrorList.add(CommonErrorLogs.builder()
                                .entityId(shipId)
                                .entityType(Constants.SHIPMENT)
                                .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                                .errorMessage(sendConsoleValidationResponse.getShipmentErrorMessage())
                                .build());
                    }
                }

                // Deleted older failed scenarios
                if (!shipCommonErrorsMap.isEmpty()) {
                    Set<Long> deleteErrorIds = shipCommonErrorsMap.values().stream().map(BaseEntity::getId).collect(Collectors.toSet());
                    deleteAllById(deleteErrorIds.stream().toList());
                }
            }
            // Logged all shipment related errors
            if (!shipCommonErrorList.isEmpty()) {
                commonErrorLogsRepository.saveAll(shipCommonErrorList);
            }

            log.info("Logged Console and corresponding shipment errors in common error logs successfully.");
        }


    }

    @Override
    public void deleteAllConsoleAndShipmentErrorsLogs(Long consoleId, List<Long> shipmentIds) {
        var commonErrors = this.findByEntityIdAndEntityTypeAndErrorType(consoleId, Constants.CONSOLIDATION, CommonErrorType.AUTOMATIC_TRANSFER);
        List<CommonErrorLogs> shipCommonErrors = new ArrayList<>();
        if(CommonUtils.listIsNullOrEmpty(shipmentIds)) {
            shipCommonErrors = commonErrorLogsRepository.findByEntityIdListAndEntityTypeAndErrorType(shipmentIds, Constants.SHIPMENT, CommonErrorType.AUTOMATIC_TRANSFER.name());
        }

        List<Long> deleteIds = new ArrayList<>();
        if(CommonUtils.listIsNullOrEmpty(shipCommonErrors))
            deleteIds = commonErrors.stream().map(BaseEntity::getId).collect(Collectors.toList());
        if(CommonUtils.listIsNullOrEmpty(shipCommonErrors))
            deleteIds.addAll(shipCommonErrors.stream().map(BaseEntity::getId).toList());
        if(!deleteIds.isEmpty())
            commonErrorLogsRepository.deleteAllById(deleteIds);
    }

    @Override
    public void logShipmentAutomaticTransferErrors(SendShipmentValidationResponse sendShipmentValidationResponse, Long shipmentId) {
        var commonErrors = this.findByEntityIdAndEntityTypeAndErrorType(shipmentId, Constants.SHIPMENT, CommonErrorType.AUTOMATIC_TRANSFER);
        CommonErrorLogs commonErrorLogs;
        if (CommonUtils.listIsNullOrEmpty(commonErrors)) {
            commonErrorLogs = CommonErrorLogs.builder()
                    .entityId(shipmentId)
                    .entityType(Constants.SHIPMENT)
                    .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                    .errorMessage("Automatic file transfer has failed. " + sendShipmentValidationResponse.getShipmentErrorMessage())
                    .build();
        } else {
            commonErrorLogs = commonErrors.get(0);
            commonErrorLogs.setErrorMessage("Automatic file transfer has failed. " + sendShipmentValidationResponse.getShipmentErrorMessage());
        }

        // Logged shipment related error
        save(commonErrorLogs);
        log.info("Logged shipment errors in common error logs successfully.");
    }

    @Override
    public void deleteShipmentErrorsLogs(Long shipmentId) {
        var commonErrors = this.findByEntityIdAndEntityTypeAndErrorType(shipmentId, Constants.SHIPMENT, CommonErrorType.AUTOMATIC_TRANSFER);
        if(CommonUtils.listIsNullOrEmpty(commonErrors)) {
            commonErrorLogsRepository.deleteById(commonErrors.get(0).getId());
        }
    }

}
