package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiLegDao;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsService;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TransportInstructionLegsServiceImpl implements ITransportInstructionLegsService {
    @Autowired
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;
    @Autowired
    private ITiLegDao tiLegsDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Override
    @Transactional
    public TransportInstructionLegsResponse create(TransportInstructionLegsRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiId = request.getTiId();
        Optional<PickupDeliveryDetails> transportInstruction = pickupDeliveryDetailsRepository.findById(tiId);
        if (!transportInstruction.isPresent()) {
            throw new ValidationException("Transport Instruction does not exist for tiId: " + tiId);
        }
        validateTransportInstructionLegs(request);
        // Convert DTO to Entity
        TiLegs tiLegs = jsonHelper.convertValue(request, TiLegs.class);
        tiLegs.setPickupDeliveryDetailsId(tiId);
        log.debug("Converted Transport Instruction Legs request to entity | Entity: {}", tiLegs);
        // Save to DB
        TiLegs tiLegsEntity = tiLegsDao.save(tiLegs);
        log.info("Saved Transport Instruction Legs entity to DB | Transport Instruction Legs ID: {} | Request ID: {}", tiLegsEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(null, tiLegsEntity, DBOperationType.CREATE);
        log.info("Audit log recorded for Transport Instruction Legs creation | Transport Instruction Legs ID: {}", tiLegsEntity.getId());

        TransportInstructionLegsResponse response = jsonHelper.convertValue(tiLegsEntity, TransportInstructionLegsResponse.class);
        log.info("Returning Transport Instruction Legs response | Transport Instruction Legs ID: {} | Response: {}", tiLegsEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegsEntity.getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    @Transactional
    public TransportInstructionLegsResponse update(TransportInstructionLegsRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs update | Request ID: {} | Request Body: {}", requestId, request);
        Long id = request.getId();
        Optional<TiLegs> existingTiLegs = tiLegsDao.findById(id);
        if (!existingTiLegs.isPresent()) {
            throw new ValidationException("Invalid Transport Instruction Legs id" + id);
        }
        validateTransportInstructionLegs(request);
        Long tiId = request.getTiId();
        Optional<PickupDeliveryDetails> transportInstruction = pickupDeliveryDetailsRepository.findById(tiId);
        if (!transportInstruction.isPresent()) {
            throw new ValidationException("Transport Instruction does not exist for tiId: " + tiId);
        }
        TiLegs existingTiLegsEntity = existingTiLegs.get();
        // Convert DTO to Entity
        TiLegs tiLegs = jsonHelper.convertValue(request, TiLegs.class);
        tiLegs.setTiContainers(existingTiLegsEntity.getTiContainers());
        tiLegs.setTiPackages(existingTiLegsEntity.getTiPackages());
        tiLegs.setTiReferences(existingTiLegsEntity.getTiReferences());
        tiLegs.setTiTruckDriverDetails(existingTiLegsEntity.getTiTruckDriverDetails());
        tiLegs.setPickupDeliveryDetailsId(tiId);
        log.debug("Converted Transport Instruction Legs update request to entity | Entity: {}", tiLegs);
        // Save to DB
        TiLegs tiLegsEntity = tiLegsDao.save(tiLegs);
        log.info("Updated Transport Instruction Legs entity to DB | Transport Instruction Legs ID: {} | Request ID: {}", tiLegsEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(existingTiLegsEntity, tiLegsEntity, DBOperationType.UPDATE);
        log.info("Audit log recorded for Transport Instruction Legs Update | Transport Instruction Legs ID: {}", tiLegsEntity.getId());

        TransportInstructionLegsResponse response = jsonHelper.convertValue(tiLegsEntity, TransportInstructionLegsResponse.class);
        log.info("Returning Transport Instruction Legs response | Transport Instruction Legs ID: {} | Response: {}", tiLegsEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegsEntity.getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsListResponse list(ListCommonRequest request) {
        // construct specifications for filter request
        Pair<Specification<TiLegs>, Pageable> tuple = fetchData(request, TiLegs.class);
        Page<TiLegs> tiLegsPage = tiLegsDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Transport Instruction Legs list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        TransportInstructionLegsListResponse transportInstructionLegsListResponse = new TransportInstructionLegsListResponse();
        if (tiLegsPage != null) {
            List<TransportInstructionLegsResponse> responseList = convertEntityListToDtoList(tiLegsPage.getContent());
            transportInstructionLegsListResponse.setTiLegsResponses(responseList);
            transportInstructionLegsListResponse.setTotalPages(tiLegsPage.getTotalPages());
            transportInstructionLegsListResponse.setTotalCount(tiLegsPage.getTotalElements());
        }

        return transportInstructionLegsListResponse;
    }

    @Override
    @Transactional
    public TransportInstructionLegsResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Optional<TiLegs> tiLegs = tiLegsDao.findById(id);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Invalid Ti legs Id: " + id);
        }
        TiLegs tiLegsEntity = tiLegs.get();
        tiLegsDao.delete(tiLegsEntity);

        recordAuditLogs(tiLegsEntity, null, DBOperationType.DELETE);

        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegsEntity.getPickupDeliveryDetailsId());
        return new TransportInstructionLegsResponse();
    }

    @Override
    public TransportInstructionLegsResponse retrieveById(Long id) {
        Optional<TiLegs> tiLegs = tiLegsDao.findById(id);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Invalid Ti legs Id: " + id);
        }
        return jsonHelper.convertValue(tiLegs.get(), TransportInstructionLegsResponse.class);
    }

    private void recordAuditLogs(TiLegs oldTiLegs, TiLegs newTiLegs, DBOperationType operationType) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .tenantId(UserContext.getUser().getTenantId())
                        .userName(UserContext.getUser().getUsername())
                        .prevData(oldTiLegs)
                        .newData(newTiLegs)
                        .parent(PickupDeliveryDetails.class.getSimpleName())
                        .parentId(newTiLegs != null ? newTiLegs.getPickupDeliveryDetailsId() : oldTiLegs.getPickupDeliveryDetailsId())
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

    private List<TransportInstructionLegsResponse> convertEntityListToDtoList(List<TiLegs> contents) {
        List<TransportInstructionLegsResponse> responseList = new ArrayList<>();
        contents.forEach(tiLegs -> {
            TransportInstructionLegsResponse response = convertEntityToDto(tiLegs);
            responseList.add(response);
        });
        return responseList;
    }

    private TransportInstructionLegsResponse convertEntityToDto(TiLegs tiLegs) {
        return jsonHelper.convertValue(tiLegs, TransportInstructionLegsResponse.class);
    }

    private void validateTransportInstructionLegs(TransportInstructionLegsRequest transportInstructionLegsRequest) {
        PartiesRequest origin = transportInstructionLegsRequest.getOrigin();
        PartiesRequest destination = transportInstructionLegsRequest.getDestination();
        if (!Objects.isNull(origin)) {
            validateOrgAndAddress(origin);
        }
        if (!Objects.isNull(destination)) {
            validateOrgAndAddress(destination);
        }
        validateDates(transportInstructionLegsRequest);
    }

    private static void validateOrgAndAddress(PartiesRequest origin) {
        boolean hasOrgInfo = StringUtility.isNotEmpty(origin.getOrgId())
                || StringUtility.isNotEmpty(origin.getOrgCode())
                || !CollectionUtils.isEmpty(origin.getOrgData());
        boolean isOrgInfoMissing = StringUtility.isEmpty(origin.getOrgId())
                && StringUtility.isEmpty(origin.getOrgCode())
                && CollectionUtils.isEmpty(origin.getOrgData());
        boolean isAddressInfoMissing = StringUtility.isEmpty(origin.getAddressCode())
                || StringUtility.isEmpty(origin.getAddressId())
                || CollectionUtils.isEmpty(origin.getAddressData());
        boolean hasAddressInfo = StringUtility.isNotEmpty(origin.getAddressCode())
                && StringUtility.isNotEmpty(origin.getAddressId())
                && !CollectionUtils.isEmpty(origin.getAddressData());

        if (hasOrgInfo) {
            if (isAddressInfoMissing) {
                throw new ValidationException("Please provide the origin address info");
            } else if (isOrgInfoMissing) {
                throw new ValidationException("Please provide the complete org details");
            }
        } else if (hasAddressInfo) {
            throw new ValidationException("Please provide the org details");
        }
    }

    public void validateDates(TransportInstructionLegsRequest transportInstructionLegsRequest) {
        if (transportInstructionLegsRequest.getEstimatedPickup() != null && transportInstructionLegsRequest.getEstimatedDelivery() != null) {
            if (transportInstructionLegsRequest.getEstimatedPickup().isAfter(transportInstructionLegsRequest.getEstimatedDelivery())) {
                throw new ValidationException("Estimated Pickup cannot be later than Estimated Delivery.");
            }
            if (transportInstructionLegsRequest.getEstimatedDelivery().isBefore(transportInstructionLegsRequest.getEstimatedPickup())) {
                throw new ValidationException("Estimated Delivery cannot be earlier than Estimated Pickup.");
            }
        }

        if (transportInstructionLegsRequest.getActualPickup() != null && transportInstructionLegsRequest.getActualDelivery() != null) {
            if (transportInstructionLegsRequest.getActualPickup().isAfter(transportInstructionLegsRequest.getActualDelivery())) {
                throw new ValidationException("Actual Pickup cannot be later than Actual Delivery.");
            }
            if (transportInstructionLegsRequest.getActualDelivery().isBefore(transportInstructionLegsRequest.getActualPickup())) {
                throw new ValidationException("Actual Delivery cannot be earlier than Actual Pickup.");
            }
        }
    }
}
