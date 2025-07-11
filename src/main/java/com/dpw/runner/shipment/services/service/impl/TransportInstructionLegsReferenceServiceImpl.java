package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiReferenceDao;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsReferenceListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsReferenceRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsReferenceResponse;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsReferenceService;
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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TransportInstructionLegsReferenceServiceImpl implements ITransportInstructionLegsReferenceService {
    private static final String TI_LEGS_DOES_NOT_EXIST = "Transport Instruction Legs does not exist for tiId: ";
    @Autowired
    private ITiLegRepository tiLegRepository;
    @Autowired
    private ITiReferenceDao tiReferenceDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;
    @Override
    public TransportInstructionLegsReferenceResponse create(TransportInstructionLegsReferenceRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs reference creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (tiLegs.isEmpty()) {
            throw new ValidationException(TI_LEGS_DOES_NOT_EXIST + tiLegId);
        }
        validateTransportInstructionLegsReferenceNumbers(request);
        // Convert DTO to Entity
        TiReferences tiReferences = jsonHelper.convertValue(request, TiReferences.class);
        tiReferences.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs reference request to entity | Entity: {}", tiReferences);
        // Save to DB
        TiReferences tiReferenceEntity = tiReferenceDao.save(tiReferences);
        log.info("Saved Transport Instruction Legs reference entity to DB | Transport Instruction Legs reference ID: {} | Request ID: {}", tiReferenceEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(null, tiReferenceEntity, DBOperationType.CREATE);
        log.info("Audit log recorded for Transport Instruction Legs reference creation | Transport Instruction Legs reference ID: {}", tiReferenceEntity.getId());

        TransportInstructionLegsReferenceResponse response = jsonHelper.convertValue(tiReferenceEntity, TransportInstructionLegsReferenceResponse.class);
        log.info("Returning Transport Instruction Legs reference response | Transport Instruction Legs reference ID: {} | Response: {}", tiReferenceEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsReferenceResponse update(TransportInstructionLegsReferenceRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs reference update | Request ID: {} | Request Body: {}", requestId, request);
        Long id = request.getId();
        Optional<TiReferences> existingTiLegsReference = tiReferenceDao.findById(id);
        if (!existingTiLegsReference.isPresent()) {
            throw new ValidationException("Invalid Transport Instruction Legs reference id" + id);
        }
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (tiLegs.isEmpty()) {
            throw new ValidationException(TI_LEGS_DOES_NOT_EXIST + tiLegId);
        }
        validateTransportInstructionLegsReferenceNumbers(request);
        // Convert DTO to Entity
        TiReferences tiReferences = jsonHelper.convertValue(request, TiReferences.class);
        tiReferences.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs reference update request to entity | Entity: {}", tiLegs);
        // Save to DB
        TiReferences tiReferenceEntity = tiReferenceDao.save(tiReferences);
        log.info("Updated Transport Instruction Legs reference entity to DB | Transport Instruction Legs reference ID: {} | Request ID: {}", tiReferenceEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(existingTiLegsReference.get(), tiReferenceEntity, DBOperationType.UPDATE);
        log.info("Audit log recorded for Transport Instruction Legs reference Update | Transport Instruction Legs reference ID: {}", tiReferenceEntity.getId());

        TransportInstructionLegsReferenceResponse response = jsonHelper.convertValue(tiReferenceEntity, TransportInstructionLegsReferenceResponse.class);
        log.info("Returning Transport Instruction Legs reference response | Transport Instruction Legs reference ID: {} | Response: {}", tiReferenceEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsReferenceListResponse list(ListCommonRequest request) {
        // construct specifications for filter request
        Pair<Specification<TiReferences>, Pageable> tuple = fetchData(request, TiReferences.class);
        Page<TiReferences> tiLegsReferencePage = tiReferenceDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Transport Instruction Legs reference list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        TransportInstructionLegsReferenceListResponse transportInstructionLegsReferenceListResponse = new TransportInstructionLegsReferenceListResponse();
        if (tiLegsReferencePage != null) {
            List<TransportInstructionLegsReferenceResponse> responseList = convertEntityListToDtoList(tiLegsReferencePage.getContent());
            transportInstructionLegsReferenceListResponse.setTiLegsReferenceResponses(responseList);
            transportInstructionLegsReferenceListResponse.setTotalPages(tiLegsReferencePage.getTotalPages());
            transportInstructionLegsReferenceListResponse.setTotalCount(tiLegsReferencePage.getTotalElements());
        }

        return transportInstructionLegsReferenceListResponse;
    }

    @Override
    public TransportInstructionLegsReferenceResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        Optional<TiReferences> tiReferences = tiReferenceDao.findById(id);
        if (tiReferences.isEmpty()) {
            throw new ValidationException("Invalid Ti legs reference Id: " + id);
        }
        TiReferences tiReferenceEntity = tiReferences.get();

        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiReferenceEntity.getTiLegId());
        tiReferenceDao.delete(tiReferenceEntity);

        recordAuditLogs(tiReferenceEntity, null, DBOperationType.DELETE);

        // Triggering Event for shipment and console for DependentServices update
        if (tiLegs.isPresent()) {
            log.info("Publishing info reference TI to down stream {}", tiLegs.get().getPickupDeliveryDetailsId());
            triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        }
        return new TransportInstructionLegsReferenceResponse();
    }

    @Override
    public TransportInstructionLegsReferenceResponse retrieveById(Long id) {
        Optional<TiReferences> tiReferences = tiReferenceDao.findById(id);
        if (tiReferences.isEmpty()) {
            throw new ValidationException("Invalid Ti legs reference Id: " + id);
        }
        return jsonHelper.convertValue(tiReferences.get(), TransportInstructionLegsReferenceResponse.class);
    }

    @Override
    @Transactional
    public TransportInstructionLegsReferenceListResponse bulkCreate(TransportInstructionLegsReferenceListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs reference creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiLegId = request.getReferences().get(0).getTiLegId();
        if (!request.getReferences().stream()
                .allMatch(req -> req.getTiLegId().equals(tiLegId))) {
            throw new ValidationException("All tiLegId values must be the same");
        }

        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (tiLegs.isEmpty()) {
            throw new ValidationException(TI_LEGS_DOES_NOT_EXIST + tiLegId);
        }
        request.getReferences()
                .forEach(this::validateTransportInstructionLegsReferenceNumbers);
        // Convert DTO to Entity
        List<TiReferences> tiReferencesList = new ArrayList<>();
        for (TransportInstructionLegsReferenceRequest referenceRequest : request.getReferences()) {
            TiReferences tiReferences = jsonHelper.convertValue(referenceRequest, TiReferences.class);
            tiReferences.setTiLegId(tiLegId);
            tiReferencesList.add(tiReferences);
            log.info("Converted Transport Instruction Legs reference request to entity | Entity: {}", tiReferences);
        }
        // Save to DB
        List<TiReferences> referencesList = tiReferenceDao.saveAll(tiReferencesList);
        // Audit logging
        for (TiReferences tiReferenceEntity : referencesList) {
            recordAuditLogs(null, tiReferenceEntity, DBOperationType.CREATE);
            log.info("Audit log recorded for Transport Instruction Legs reference creation | Transport Instruction Legs reference ID: {}", tiReferenceEntity.getId());
        }
        TransportInstructionLegsReferenceListResponse response = new TransportInstructionLegsReferenceListResponse();
        List<TransportInstructionLegsReferenceResponse> instructionLegsReferenceResponses = new ArrayList<>();
        for (TiReferences tiReferenceEntity : referencesList) {
            TransportInstructionLegsReferenceResponse referenceResponse = jsonHelper.convertValue(tiReferenceEntity, TransportInstructionLegsReferenceResponse.class);
            instructionLegsReferenceResponses.add(referenceResponse);
        }
        response.setTiLegsReferenceResponses(instructionLegsReferenceResponses);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    private void recordAuditLogs(TiReferences oldTiReference, TiReferences newTiReference, DBOperationType operationType) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .tenantId(UserContext.getUser().getTenantId())
                        .userName(UserContext.getUser().getUsername())
                        .prevData(oldTiReference)
                        .newData(newTiReference)
                        .parent(TiLegs.class.getSimpleName())
                        .parentId(newTiReference != null ? newTiReference.getTiLegId() : oldTiReference.getTiLegId())
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

    private List<TransportInstructionLegsReferenceResponse> convertEntityListToDtoList(List<TiReferences> contents) {
        List<TransportInstructionLegsReferenceResponse> responseList = new ArrayList<>();
        contents.forEach(tiLegs -> {
            TransportInstructionLegsReferenceResponse response = convertEntityToDto(tiLegs);
            responseList.add(response);
        });
        return responseList;
    }

    private TransportInstructionLegsReferenceResponse convertEntityToDto(TiReferences tiLegs) {
        return jsonHelper.convertValue(tiLegs, TransportInstructionLegsReferenceResponse.class);
    }

    private void validateTransportInstructionLegsReferenceNumbers(TransportInstructionLegsReferenceRequest transportInstructionLegsReferenceRequest) {
        if (StringUtility.isNotEmpty(transportInstructionLegsReferenceRequest.getType()) && StringUtility.isEmpty(transportInstructionLegsReferenceRequest.getReference())) {
            throw new ValidationException("Missing Reference when Type is selected");
        } else if (StringUtility.isEmpty(transportInstructionLegsReferenceRequest.getType()) && StringUtility.isNotEmpty(transportInstructionLegsReferenceRequest.getReference())) {
            throw new ValidationException("Missing Type when Reference is present");
        } else if (StringUtility.isEmpty(transportInstructionLegsReferenceRequest.getType()) && StringUtility.isEmpty(transportInstructionLegsReferenceRequest.getReference())) {
            throw new ValidationException("Missing both type and reference number");
        }
    }
}
