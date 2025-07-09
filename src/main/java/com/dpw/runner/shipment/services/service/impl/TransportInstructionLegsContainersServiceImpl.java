package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiContainerDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersResponse;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsContainersService;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TransportInstructionLegsContainersServiceImpl implements ITransportInstructionLegsContainersService {
    @Autowired
    private ITiLegRepository tiLegRepository;
    @Autowired
    private ITiContainerDao tiContainerDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;
    @Autowired
    private IContainerV3Service containerV3Service;

    @Override
    public TransportInstructionLegsContainersResponse create(TransportInstructionLegsContainersRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs containers creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        TiLegs tiLegsEntity = tiLegs.get();
        validateDuplicateContainerNumberInLeg(tiLegsEntity.getTiContainers(), request.getNumber(), request.getId());
        validateTransportInstructionLegsContainersDetails(request);
        // Convert DTO to Entity
        TiContainers tiContainers = jsonHelper.convertValue(request, TiContainers.class);
        tiContainers.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs container request to entity | Entity: {}", tiContainers);
        // Save to DB
        TiContainers tiContainersEntity = tiContainerDao.save(tiContainers);
        log.info("Saved Transport Instruction Legs container entity to DB | Transport Instruction Legs ID: {} | Request ID: {}", tiContainersEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(null, tiContainersEntity, DBOperationType.CREATE);
        log.info("Audit log recorded for Transport Instruction Legs container creation | Transport Instruction Legs ID: {}", tiContainersEntity.getId());

        TransportInstructionLegsContainersResponse response = jsonHelper.convertValue(tiContainersEntity, TransportInstructionLegsContainersResponse.class);
        log.info("Returning Transport Instruction Legs container response | Transport Instruction Legs container ID: {} | Response: {}", tiContainersEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegsEntity.getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsContainersResponse update(TransportInstructionLegsContainersRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs containers update | Request ID: {} | Request Body: {}", requestId, request);
        Long id = request.getId();
        Optional<TiContainers> existingTiLegsContainers = tiContainerDao.findById(id);
        if (!existingTiLegsContainers.isPresent()) {
            throw new ValidationException("Invalid Transport Instruction Legs containers id" + id);
        }
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        TiLegs tiLegsEntity = tiLegs.get();
        validateDuplicateContainerNumberInLeg(tiLegsEntity.getTiContainers(), request.getNumber(), request.getId());
        validateTransportInstructionLegsContainersDetails(request);
        // Convert DTO to Entity
        TiContainers tiContainers = jsonHelper.convertValue(request, TiContainers.class);
        tiContainers.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs containers update request to entity | Entity: {}", tiLegs);
        // Save to DB
        TiContainers tiContainersEntity = tiContainerDao.save(tiContainers);
        log.info("Updated Transport Instruction Legs containers entity to DB | Transport Instruction Legs containers ID: {} | Request ID: {}", tiContainersEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(existingTiLegsContainers.get(), tiContainersEntity, DBOperationType.UPDATE);
        log.info("Audit log recorded for Transport Instruction Legs containers Update | Transport Instruction Legs containers ID: {}", tiContainersEntity.getId());

        TransportInstructionLegsContainersResponse response = jsonHelper.convertValue(tiContainersEntity, TransportInstructionLegsContainersResponse.class);
        log.info("Returning Transport Instruction Legs containers response | Transport Instruction Legs containers ID: {} | Response: {}", tiContainersEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegsEntity.getPickupDeliveryDetailsId());
        return response;
    }

    private void validateDuplicateContainerNumberInLeg(List<TiContainers> tiContainers, String number, Long id) {
        if (!CollectionUtils.isEmpty(tiContainers)) {
            for (TiContainers containers : tiContainers) {
                if (!Objects.equals(containers.getId(), id) && StringUtility.isNotEmpty(containers.getNumber()) && containers.getNumber().equals(number)) {
                    throw new ValidationException("Container Number cannot be same for two different containers in same legs");
                }
            }
        }
    }

    @Override
    public TransportInstructionLegsContainersListResponse list(ListCommonRequest request) {

        // construct specifications for filter request
        Pair<Specification<TiContainers>, Pageable> tuple = fetchData(request, TiContainers.class);
        Page<TiContainers> tiLegsPage = tiContainerDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Transport Instruction Legs containers list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        TransportInstructionLegsContainersListResponse transportInstructionLegsContainersListResponse = new TransportInstructionLegsContainersListResponse();
        if (tiLegsPage != null) {
            List<TransportInstructionLegsContainersResponse> responseList = convertEntityListToDtoList(tiLegsPage.getContent());
            transportInstructionLegsContainersListResponse.setTiLegsContainersResponses(responseList);
            transportInstructionLegsContainersListResponse.setTotalPages(tiLegsPage.getTotalPages());
            transportInstructionLegsContainersListResponse.setTotalCount(tiLegsPage.getTotalElements());
        }

        return transportInstructionLegsContainersListResponse;
    }

    @Override
    public TransportInstructionLegsContainersResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        Optional<TiContainers> tiContainers = tiContainerDao.findById(id);
        if (!tiContainers.isPresent()) {
            throw new ValidationException("Invalid Ti legs container Id: " + id);
        }
        TiContainers tiContainersEntity = tiContainers.get();

        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiContainersEntity.getTiLegId());
        tiContainerDao.delete(tiContainersEntity);

        recordAuditLogs(tiContainersEntity, null, DBOperationType.DELETE);

        // Triggering Event for shipment and console for DependentServices update
        if (tiLegs.isPresent()) {
            log.info("Publishing info containers TI to down stream {}", tiLegs.get().getPickupDeliveryDetailsId());
            triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        }
        return new TransportInstructionLegsContainersResponse();
    }

    @Override
    public TransportInstructionLegsContainersResponse retrieveById(Long id) {

        Optional<TiContainers> tiContainers = tiContainerDao.findById(id);
        if (!tiContainers.isPresent()) {
            throw new ValidationException("Invalid Ti legs container Id: " + id);
        }
        return jsonHelper.convertValue(tiContainers.get(), TransportInstructionLegsContainersResponse.class);
    }

    @Override
    public TransportInstructionLegsContainersListResponse bulkCreate(TransportInstructionLegsContainersListRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs containers creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiLegId = request.getContainersRequests().get(0).getTiLegId();
        if (!request.getContainersRequests().stream()
                .allMatch(req -> req.getTiLegId().equals(tiLegId))) {
            throw new ValidationException("All tiLegId values must be the same");
        }

        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        TiLegs tiLegsEntity = tiLegs.get();
        request.getContainersRequests().forEach(containersRequest -> {
            validateDuplicateContainerNumberInLeg(tiLegsEntity.getTiContainers(), containersRequest.getNumber(), containersRequest.getId());
            validateTransportInstructionLegsContainersDetails(containersRequest);
        });
        // Convert DTO to Entity
        List<TiContainers> tiContainersList = new ArrayList<>();
        for (TransportInstructionLegsContainersRequest containersRequest : request.getContainersRequests()) {
            TiContainers tiContainers = jsonHelper.convertValue(containersRequest, TiContainers.class);
            tiContainers.setTiLegId(tiLegId);
            tiContainersList.add(tiContainers);
            log.debug("Converted Transport Instruction Legs container request to entity | Entity: {}", tiContainers);
        }
        // Save to DB
        List<TiContainers> tiContainersEntities = tiContainerDao.saveAll(tiContainersList);

        // Audit logging
        for (TiContainers tiContainersEntity : tiContainersEntities) {
            recordAuditLogs(null, tiContainersEntity, DBOperationType.CREATE);
            log.info("Audit log recorded for Transport Instruction Legs container creation | Transport Instruction Legs ID: {}", tiContainersEntity.getId());
        }
        TransportInstructionLegsContainersListResponse response = new TransportInstructionLegsContainersListResponse();
        List<TransportInstructionLegsContainersResponse> instructionLegsContainersResponses = new ArrayList<>();
        for (TiContainers tiContainersEntity : tiContainersEntities) {
            TransportInstructionLegsContainersResponse packagesResponse = jsonHelper.convertValue(tiContainersEntity, TransportInstructionLegsContainersResponse.class);
            instructionLegsContainersResponses.add(packagesResponse);
        }
        response.setTiLegsContainersResponses(instructionLegsContainersResponses);

        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegsEntity.getPickupDeliveryDetailsId());
        return response;
    }

    private void recordAuditLogs(TiContainers oldTiContainer, TiContainers newTiContainers, DBOperationType operationType) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .tenantId(UserContext.getUser().getTenantId())
                        .userName(UserContext.getUser().getUsername())
                        .prevData(oldTiContainer)
                        .newData(newTiContainers)
                        .parent(TiLegs.class.getSimpleName())
                        .parentId(newTiContainers != null ? newTiContainers.getTiLegId() : oldTiContainer.getTiLegId())
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

    private List<TransportInstructionLegsContainersResponse> convertEntityListToDtoList(List<TiContainers> contents) {
        List<TransportInstructionLegsContainersResponse> responseList = new ArrayList<>();
        contents.forEach(tiLegs -> {
            TransportInstructionLegsContainersResponse response = convertEntityToDto(tiLegs);
            responseList.add(response);
        });
        return responseList;
    }

    private TransportInstructionLegsContainersResponse convertEntityToDto(TiContainers tiLegs) {
        return jsonHelper.convertValue(tiLegs, TransportInstructionLegsContainersResponse.class);
    }

    private void validateTransportInstructionLegsContainersDetails(TransportInstructionLegsContainersRequest transportInstructionLegsContainersRequest) {

        if (StringUtility.isNotEmpty(transportInstructionLegsContainersRequest.getNumber())) {
            ContainerNumberCheckResponse containerNumberCheckResponse = containerV3Service.validateContainerNumber(transportInstructionLegsContainersRequest.getNumber());
            if (containerNumberCheckResponse == null || !containerNumberCheckResponse.isSuccess()) {
                throw new ValidationException("Invalid container number format");
            }
        }
        if ((transportInstructionLegsContainersRequest.getGrossWeight() != null && StringUtility.isEmpty(transportInstructionLegsContainersRequest.getGrossWeightUnit())) ||
                (transportInstructionLegsContainersRequest.getGrossWeight() == null && StringUtility.isNotEmpty(transportInstructionLegsContainersRequest.getGrossWeightUnit()))) {
            throw new ValidationException("Containers: Gross weight and gross weight unit must both be provided or both be null.");
        }
        if ((transportInstructionLegsContainersRequest.getNoOfPackages() != null && StringUtility.isEmpty(transportInstructionLegsContainersRequest.getPackageType())) ||
                (transportInstructionLegsContainersRequest.getNoOfPackages() == null && StringUtility.isNotEmpty(transportInstructionLegsContainersRequest.getPackageType()))) {
            throw new ValidationException("Containers: No of packages and package type must both be provided or both be null.");
        }

        validateNetWeight(transportInstructionLegsContainersRequest);

        validateVolume(transportInstructionLegsContainersRequest);
    }

    private static void validateVolume(TransportInstructionLegsContainersRequest transportInstructionLegsContainersRequest) {
        // Volume & Unit Validation
        if ((transportInstructionLegsContainersRequest.getVolume() != null && StringUtility.isEmpty(transportInstructionLegsContainersRequest.getVolumeUnit())) ||
                (transportInstructionLegsContainersRequest.getVolume() == null && StringUtility.isNotEmpty(transportInstructionLegsContainersRequest.getVolumeUnit()))) {
            throw new ValidationException("Containers: Volume and volume unit must both be provided or both be null.");
        }
    }

    private static void validateNetWeight(TransportInstructionLegsContainersRequest transportInstructionLegsContainersRequest) {
        // Net Weight & Unit Validation
        if ((transportInstructionLegsContainersRequest.getNetWeight() != null && StringUtility.isEmpty(transportInstructionLegsContainersRequest.getNetWeightUnit())) ||
                (transportInstructionLegsContainersRequest.getNetWeight() == null && StringUtility.isNotEmpty(transportInstructionLegsContainersRequest.getNetWeightUnit()))) {
            throw new ValidationException("Containers: Net weight and net weight unit must both be provided or both be null.");
        }
    }
}
