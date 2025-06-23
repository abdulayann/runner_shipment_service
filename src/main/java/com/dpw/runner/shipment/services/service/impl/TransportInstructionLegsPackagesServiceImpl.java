package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiPackageDao;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsPackagesService;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TransportInstructionLegsPackagesServiceImpl implements ITransportInstructionLegsPackagesService {
    @Autowired
    private ITiLegRepository tiLegRepository;
    @Autowired
    private ITiPackageDao tiPackageDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;
    private static final Pattern DIMENSION_PATTERN = Pattern.compile(
            "^\\s*(\\d+(\\.\\d+)?)\\s*[xX×]\\s*(\\d+(\\.\\d+)?)\\s*[xX×]\\s*(\\d+(\\.\\d+)?)\\s*$"
    );


    @Override
    public TransportInstructionLegsPackagesResponse create(TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs packages creation | Request ID: {} | Request Body: {}", requestId, request);
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        validateTransportInstructionLegsPackagesDetails(request);
        // Convert DTO to Entity
        TiPackages tiPackages = jsonHelper.convertValue(request, TiPackages.class);
        tiPackages.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs packages request to entity | Entity: {}", tiPackages);
        // Save to DB
        TiPackages tiPackagesEntity = tiPackageDao.save(tiPackages);
        log.info("Saved Transport Instruction Legs packages entity to DB | Transport Instruction Legs package ID: {} | Request ID: {}", tiPackagesEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(null, tiPackagesEntity, DBOperationType.CREATE);
        log.info("Audit log recorded for Transport Instruction Legs packages creation | Transport Instruction Legs package ID: {}", tiPackagesEntity.getId());

        TransportInstructionLegsPackagesResponse response = jsonHelper.convertValue(tiPackagesEntity, TransportInstructionLegsPackagesResponse.class);
        log.info("Returning Transport Instruction Legs packages response | Transport Instruction Legs package ID: {} | Response: {}", tiPackagesEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsPackagesResponse update(TransportInstructionLegsPackagesRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting Transport Instruction Legs packages update | Request ID: {} | Request Body: {}", requestId, request);
        Long id = request.getId();
        Optional<TiPackages> existingTiLegsPackages = tiPackageDao.findById(id);
        if (!existingTiLegsPackages.isPresent()) {
            throw new ValidationException("Invalid Transport Instruction Legs packages id" + id);
        }
        Long tiLegId = request.getTiLegId();
        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiLegId);
        if (!tiLegs.isPresent()) {
            throw new ValidationException("Transport Instruction Legs does not exist for tiId: " + tiLegId);
        }
        validateTransportInstructionLegsPackagesDetails(request);
        // Convert DTO to Entity
        TiPackages tiPackages = jsonHelper.convertValue(request, TiPackages.class);
        tiPackages.setTiLegId(tiLegId);
        log.debug("Converted Transport Instruction Legs packages update request to entity | Entity: {}", tiLegs);
        // Save to DB
        TiPackages tiPackagesEntity = tiPackageDao.save(tiPackages);
        log.info("Updated Transport Instruction Legs packages entity to DB | Transport Instruction Legs package ID: {} | Request ID: {}", tiPackagesEntity.getId(), requestId);

        // Audit logging
        recordAuditLogs(existingTiLegsPackages.get(), tiPackagesEntity, DBOperationType.UPDATE);
        log.info("Audit log recorded for Transport Instruction Legs packages Update | Transport Instruction Legs package ID: {}", tiPackagesEntity.getId());

        TransportInstructionLegsPackagesResponse response = jsonHelper.convertValue(tiPackagesEntity, TransportInstructionLegsPackagesResponse.class);
        log.info("Returning Transport Instruction Legs packages response | Transport Instruction Legs packages ID: {} | Response: {}", tiPackagesEntity.getId(), response);
        // Triggering Event for shipment and console for DependentServices update
        triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        return response;
    }

    @Override
    public TransportInstructionLegsPackagesListResponse list(ListCommonRequest request) {

        // construct specifications for filter request
        Pair<Specification<TiPackages>, Pageable> tuple = fetchData(request, TiPackages.class);
        Page<TiPackages> tiLegsPackagesPage = tiPackageDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Transport Instruction Legs packages list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        TransportInstructionLegsPackagesListResponse transportInstructionLegsPackagesListResponse = new TransportInstructionLegsPackagesListResponse();
        if (tiLegsPackagesPage != null) {
            List<TransportInstructionLegsPackagesResponse> responseList = convertEntityListToDtoList(tiLegsPackagesPage.getContent());
            transportInstructionLegsPackagesListResponse.setTiLegsPackagesResponses(responseList);
            transportInstructionLegsPackagesListResponse.setTotalPages(tiLegsPackagesPage.getTotalPages());
            transportInstructionLegsPackagesListResponse.setTotalCount(tiLegsPackagesPage.getTotalElements());
        }

        return transportInstructionLegsPackagesListResponse;
    }

    @Override
    public TransportInstructionLegsPackagesResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {

        Optional<TiPackages> tiPackages = tiPackageDao.findById(id);
        if (!tiPackages.isPresent()) {
            throw new ValidationException("Invalid Ti legs package Id: " + id);
        }
        TiPackages tiPackagesEntity = tiPackages.get();

        Optional<TiLegs> tiLegs = tiLegRepository.findById(tiPackagesEntity.getTiLegId());
        tiPackageDao.delete(tiPackagesEntity);

        recordAuditLogs(tiPackagesEntity, null, DBOperationType.DELETE);

        // Triggering Event for shipment and console for DependentServices update
        if (tiLegs.isPresent()) {
            log.info("Publishing info packages TI to down stream {}", tiLegs.get().getPickupDeliveryDetailsId());
            triggerPushToDownStreamForTransportInstruction(tiLegs.get().getPickupDeliveryDetailsId());
        }
        return new TransportInstructionLegsPackagesResponse();
    }

    @Override
    public TransportInstructionLegsPackagesResponse retrieveById(Long id) {

        Optional<TiPackages> tiPackages = tiPackageDao.findById(id);
        if (!tiPackages.isPresent()) {
            throw new ValidationException("Invalid Ti legs package Id: " + id);
        }
        return jsonHelper.convertValue(tiPackages.get(), TransportInstructionLegsPackagesResponse.class);
    }

    private void recordAuditLogs(TiPackages oldTiPackages, TiPackages newTiPackages, DBOperationType operationType) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .tenantId(UserContext.getUser().getTenantId())
                        .userName(UserContext.getUser().getUsername())
                        .prevData(oldTiPackages)
                        .newData(newTiPackages)
                        .parent(TiLegs.class.getSimpleName())
                        .parentId(newTiPackages != null ? newTiPackages.getTiLegId() : oldTiPackages.getTiLegId())
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

    private List<TransportInstructionLegsPackagesResponse> convertEntityListToDtoList(List<TiPackages> contents) {
        List<TransportInstructionLegsPackagesResponse> responseList = new ArrayList<>();
        contents.forEach(tiLegs -> {
            TransportInstructionLegsPackagesResponse response = convertEntityToDto(tiLegs);
            responseList.add(response);
        });
        return responseList;
    }

    private TransportInstructionLegsPackagesResponse convertEntityToDto(TiPackages tiLegs) {
        return jsonHelper.convertValue(tiLegs, TransportInstructionLegsPackagesResponse.class);
    }

    private void validateTransportInstructionLegsPackagesDetails(TransportInstructionLegsPackagesRequest transportInstructionLegsPackagesRequest) {

        validateDimensions(transportInstructionLegsPackagesRequest.getDimensions());
        if ((transportInstructionLegsPackagesRequest.getGrossWeight() != null && StringUtility.isEmpty(transportInstructionLegsPackagesRequest.getGrossWeightUnit())) ||
                (transportInstructionLegsPackagesRequest.getGrossWeight() == null && StringUtility.isNotEmpty(transportInstructionLegsPackagesRequest.getGrossWeightUnit()))) {
            throw new ValidationException("Packages: Gross weight and gross weight unit must both be provided or both be null.");
        }
        if (transportInstructionLegsPackagesRequest.getGrossWeight() != null && StringUtility.isNotEmpty(transportInstructionLegsPackagesRequest.getGrossWeightUnit())) {
            String combinedGrossWeight = transportInstructionLegsPackagesRequest.getGrossWeight().toPlainString() + transportInstructionLegsPackagesRequest.getGrossWeightUnit().trim();
            if (combinedGrossWeight.length() > 15) {
                throw new ValidationException("Packages: Combined length of grossWeight and grossWeightUnit must not exceed 15 characters.");
            }
        }

        // Net Weight & Unit Validation
        validateNetWeight(transportInstructionLegsPackagesRequest);

        // Volume & Unit Validation
        validateVolume(transportInstructionLegsPackagesRequest);
    }

    private static void validateVolume(TransportInstructionLegsPackagesRequest transportInstructionLegsPackagesRequest) {
        if ((transportInstructionLegsPackagesRequest.getVolume() != null && StringUtility.isEmpty(transportInstructionLegsPackagesRequest.getVolumeUnit())) ||
                (transportInstructionLegsPackagesRequest.getVolume() == null && StringUtility.isNotEmpty(transportInstructionLegsPackagesRequest.getVolumeUnit()))) {
            throw new ValidationException("Packages: Volume and volume unit must both be provided or both be null.");
        }
        if (transportInstructionLegsPackagesRequest.getVolume() != null && StringUtility.isNotEmpty(transportInstructionLegsPackagesRequest.getVolumeUnit())) {
            String combined = transportInstructionLegsPackagesRequest.getVolume().toPlainString() + transportInstructionLegsPackagesRequest.getVolumeUnit().trim();
            if (combined.length() > 10) {
                throw new ValidationException("Packages: Combined length of volume and volumeUnit must not exceed 10 characters.");
            }
        }
    }

    private static void validateNetWeight(TransportInstructionLegsPackagesRequest transportInstructionLegsPackagesRequest) {
        if ((transportInstructionLegsPackagesRequest.getNetWeight() != null && StringUtility.isEmpty(transportInstructionLegsPackagesRequest.getNetWeightUnit())) ||
                (transportInstructionLegsPackagesRequest.getNetWeight() == null && StringUtility.isNotEmpty(transportInstructionLegsPackagesRequest.getNetWeightUnit()))) {
            throw new ValidationException("Packages: Net weight and net weight unit must both be provided or both be null.");
        }
        if (transportInstructionLegsPackagesRequest.getNetWeight() != null && StringUtility.isNotEmpty(transportInstructionLegsPackagesRequest.getNetWeightUnit())) {
            String combined = transportInstructionLegsPackagesRequest.getNetWeight().toPlainString() + transportInstructionLegsPackagesRequest.getNetWeightUnit().trim();
            if (combined.length() > 15) {
                throw new ValidationException("Packages: Combined length of netWeight and netWeightUnit must not exceed 15 characters.");
            }
        }
    }

    public void validateDimensions(String dimensions) {
        if (StringUtility.isNotEmpty(dimensions)) {
            Matcher matcher = DIMENSION_PATTERN.matcher(dimensions);
            if (!matcher.matches()) {
                throw new ValidationException("Dimensions must be in the format LxWxH (e.g., 10x20x30).");
            }

            // Optional: parse values and validate further (e.g., non-zero, max size)
            double length = Double.parseDouble(matcher.group(1));
            double width = Double.parseDouble(matcher.group(3));
            double height = Double.parseDouble(matcher.group(5));

            if (length <= 0 || width <= 0 || height <= 0) {
                throw new ValidationException("Length, width, and height must be positive numbers.");
            }
        }
    }
}
