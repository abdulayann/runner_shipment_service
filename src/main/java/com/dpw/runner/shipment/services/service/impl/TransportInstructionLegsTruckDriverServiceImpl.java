package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiTruckDriverDetailDao;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsTruckDriverRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsTruckDriverService;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TransportInstructionLegsTruckDriverServiceImpl implements ITransportInstructionLegsTruckDriverService {
    @Autowired
    private ITiLegRepository tiLegRepository;
    @Autowired
    private ITiTruckDriverDetailDao tiTruckDriverDetailDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Override
    public TransportInstructionLegsTruckDriverResponse create(TransportInstructionLegsTruckDriverRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs truck driver creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        validateTransportInstructionLegsTruckDriverDetails(request);
        // Convert DTO to Entity
        TiTruckDriverDetails tiTruckDriverDetails = jsonHelper.convertValue(request, TiTruckDriverDetails.class);
        tiTruckDriverDetails.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs truck driver request to entity | Entity: {}", tiTruckDriverDetails);
        // Save to DB
        TiTruckDriverDetails tiTruckDriverDetailsEntity = tiTruckDriverDetailDao.save(tiTruckDriverDetails);
        log.info("Saved Transport Instruction Legs truck driver entity to DB | Transport Instruction Legs truck driver ID: {} | Request ID: {}", tiTruckDriverDetailsEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(null, tiTruckDriverDetailsEntity, DBOperationType.CREATE);
        log.info("Audit log recorded for Transport Instruction Legs truck driver creation | Transport Instruction Legs truck driver ID: {}", tiTruckDriverDetailsEntity.getId());

        TransportInstructionLegsTruckDriverResponse response = jsonHelper.convertValue(tiTruckDriverDetailsEntity, TransportInstructionLegsTruckDriverResponse.class);
        log.info("Returning Transport Instruction Legs truck driver response | Transport Instruction Legs truck driver ID: {} | Response: {}", tiTruckDriverDetailsEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsTruckDriverResponse update(TransportInstructionLegsTruckDriverRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs truck driver update | Request ID: {} | Request Body: {}", requestId, request);
        Long id = request.getId();
        Optional<TiTruckDriverDetails> existingTiLegsTruckDriver = tiTruckDriverDetailDao.findById(id);
        if (!existingTiLegsTruckDriver.isPresent()) {
            throw new ValidationException("Invalid Transport Instruction Legs truck driver id" + id);
        }
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        validateTransportInstructionLegsTruckDriverDetails(request);
        // Convert DTO to Entity
        TiTruckDriverDetails tiTruckDriverDetails = jsonHelper.convertValue(request, TiTruckDriverDetails.class);
        tiTruckDriverDetails.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs truck driver update request to entity | Entity: {}", tiLegs);
        // Save to DB
        TiTruckDriverDetails tiTruckDriverDetailsEntity = tiTruckDriverDetailDao.save(tiTruckDriverDetails);
        log.info("Updated Transport Instruction Legs truck driver entity to DB | Transport Instruction Legs truck driver ID: {} | Request ID: {}", tiTruckDriverDetailsEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(existingTiLegsTruckDriver.get(), tiTruckDriverDetailsEntity, DBOperationType.UPDATE);
        log.info("Audit log recorded for Transport Instruction Legs truck driver Update | Transport Instruction Legs truck driver ID: {}", tiTruckDriverDetailsEntity.getId());

        TransportInstructionLegsTruckDriverResponse response = jsonHelper.convertValue(tiTruckDriverDetailsEntity, TransportInstructionLegsTruckDriverResponse.class);
        log.info("Returning Transport Instruction Legs truck driver response | Transport Instruction Legs truck driver ID: {} | Response: {}", tiTruckDriverDetailsEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsTruckDriverListResponse list(ListCommonRequest request) {
        // construct specifications for filter request
        Pair<Specification<TiTruckDriverDetails>, Pageable> tuple = fetchData(request, TiTruckDriverDetails.class);
        Page<TiTruckDriverDetails> tiLegsTruckDriverPage = tiTruckDriverDetailDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Transport Instruction Legs truck driver list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        TransportInstructionLegsTruckDriverListResponse transportInstructionLegsTruckDriverListResponse = new TransportInstructionLegsTruckDriverListResponse();
        if (tiLegsTruckDriverPage != null) {
            List<TransportInstructionLegsTruckDriverResponse> responseList = convertEntityListToDtoList(tiLegsTruckDriverPage.getContent());
            transportInstructionLegsTruckDriverListResponse.setTiLegsTruckDriverResponses(responseList);
            transportInstructionLegsTruckDriverListResponse.setTotalPages(tiLegsTruckDriverPage.getTotalPages());
            transportInstructionLegsTruckDriverListResponse.setTotalCount(tiLegsTruckDriverPage.getTotalElements());
        }

        return transportInstructionLegsTruckDriverListResponse;
    }

    @Override
    public TransportInstructionLegsTruckDriverResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        Optional<TiTruckDriverDetails> tiTruckDriverDetails = tiTruckDriverDetailDao.findById(id);
        if (!tiTruckDriverDetails.isPresent()) {
            throw new ValidationException("Invalid Ti legs truck Driver Id: " + id);
        }
        TiTruckDriverDetails tiTruckDriverEntity = tiTruckDriverDetails.get();

        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiTruckDriverEntity.getTiLegId());
        tiTruckDriverDetailDao.delete(tiTruckDriverEntity);

        recordAuditLogs(tiTruckDriverEntity, null, DBOperationType.DELETE);

        // Triggering Event for shipment and console for DependentServices update
        if (tiLegs.isPresent()) {
            log.info("Publishing info truck driver TI to down stream {}", tiLegs.get().getPickupDeliveryDetailsId());
            triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        }
        return new TransportInstructionLegsTruckDriverResponse();
    }

    @Override
    public TransportInstructionLegsTruckDriverResponse retrieveById(Long id) {

        Optional<TiTruckDriverDetails> tiTruckDriverDetails = tiTruckDriverDetailDao.findById(id);
        if (!tiTruckDriverDetails.isPresent()) {
            throw new ValidationException("Invalid Ti legs reference Id: " + id);
        }
        return jsonHelper.convertValue(tiTruckDriverDetails.get(), TransportInstructionLegsTruckDriverResponse.class);
    }

    private void recordAuditLogs(TiTruckDriverDetails oldTiTruckDriver, TiTruckDriverDetails newTiTruckDriver, DBOperationType operationType) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .tenantId(UserContext.getUser().getTenantId())
                        .userName(UserContext.getUser().getUsername())
                        .prevData(oldTiTruckDriver)
                        .newData(newTiTruckDriver)
                        .parent(TiLegs.class.getSimpleName())
                        .parentId(newTiTruckDriver != null ? newTiTruckDriver.getTiLegId() : oldTiTruckDriver.getTiLegId())
                        .operation(operationType.name())
                        .build()
        );
    }

    private void triggerPushToDownStreamForTransportInstruction(Long transportInstructionId) {
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(transportInstructionId)
                .parentEntityName(Constants.TRANSPORT_INSTRUCTION)
                .build();
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, transportInstructionId.toString());
    }

    private List<TransportInstructionLegsTruckDriverResponse> convertEntityListToDtoList(List<TiTruckDriverDetails> contents) {
        List<TransportInstructionLegsTruckDriverResponse> responseList = new ArrayList<>();
        contents.forEach(tiLegs -> {
            TransportInstructionLegsTruckDriverResponse response = convertEntityToDto(tiLegs);
            responseList.add(response);
        });
        return responseList;
    }

    private TransportInstructionLegsTruckDriverResponse convertEntityToDto(TiTruckDriverDetails tiLegs) {
        return jsonHelper.convertValue(tiLegs, TransportInstructionLegsTruckDriverResponse.class);
    }

    private void validateTransportInstructionLegsTruckDriverDetails(TransportInstructionLegsTruckDriverRequest transportInstructionLegsTruckDriverRequest) {
        if (StringUtility.isEmpty(transportInstructionLegsTruckDriverRequest.getDriverName()) && StringUtility.isEmpty(transportInstructionLegsTruckDriverRequest.getDriverMobileNumber()) && StringUtility.isEmpty(transportInstructionLegsTruckDriverRequest.getTruckOrTrailerType())
                && StringUtility.isEmpty(transportInstructionLegsTruckDriverRequest.getTrailerNumberPlate()) && StringUtility.isEmpty(transportInstructionLegsTruckDriverRequest.getTruckNumberPlate())) {
            throw new ValidationException("At least one field of truck driver is required to save, or please discard by clicking the cancel(x)");
        }
    }

}
