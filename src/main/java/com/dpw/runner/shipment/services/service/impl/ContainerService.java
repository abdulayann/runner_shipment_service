package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ContainerPackAssignDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.StringUtility.isNotEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
public class ContainerService implements IContainerService {

    @Autowired
    IContainerDao containerDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired

    private ModelMapper modelMapper;
    private final CSVParsingUtil<Containers> parser = new CSVParsingUtil<>(Containers.class);

    @Autowired
    IEventDao eventDao;
    @Autowired
    private IPackingDao packingDao;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private AuditLogService auditLogService;
    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Autowired
    private KafkaProducer producer;

    @Value("${containersKafka.queue}")
    private String senderQueue;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Container Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Containers container = convertRequestToEntity(request);
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {
            container = containerDao.save(container);
            if (request.getPacksList() != null) {
                List<PackingRequest> packingRequest = request.getPacksList();
                List<Packing> packs = packingDao.savePacks(convertToEntityList(packingRequest, Packing.class), container.getId());
                container.setPacksList(packs);
            }
            if (eventsRequestList != null) {
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), container.getId(), Constants.CONTAINER);
                container.setEventsList(events);
            }

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(container)
                            .prevData(null)
                            .parent(Containers.class.getSimpleName())
                            .parentId(container.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Container Details Saved Successfully for Id {} with Request Id {}", container.getId(), LoggerHelper.getRequestIdFromMDC());
            afterSave(container, true);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Override
    public void uploadContainers(BulkUploadRequest request) throws Exception {
        List<Containers> containersList = parser.parseCSVFile(request.getFile());
        containersList = containersList.stream().map(c ->
                c.setConsolidationId(request.getConsolidationId())
        ).collect(Collectors.toList());
        containersList = containerDao.saveAll(containersList);
        if (request.getShipmentId() != null) {
            containersList.stream().forEach(container -> {
                shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), List.of(request.getShipmentId()));
            });
        }
        afterSaveList(containersList, true);
    }

    @Override
    public void uploadContainerEvents(BulkUploadRequest request) throws Exception {
        List<Events> eventsList = parser.parseCSVFileEvents(request.getFile());
        eventsList = eventsList.stream().map(c -> {
            c.setEntityId(request.getConsolidationId());
            c.setEntityType("CONSOLIDATION");
            return c;
        }).collect(Collectors.toList());
        eventsList = eventDao.saveAll(eventsList);
        if (request.getShipmentId() != null) {
            eventsList.stream().forEach(container -> {
                shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), List.of(request.getShipmentId()));
            });
        }
    }

    @Override
    public void downloadContainers(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws Exception {
        List<ShipmentsContainersMapping> mappings;
        List<Containers> result = new ArrayList<>();
        if (request.getShipmentId() != null) {
            List<Long> containerId = new ArrayList<>();
            mappings = shipmentsContainersMappingDao.findByShipmentId(Long.valueOf(request.getShipmentId()));
            containerId.addAll(mappings.stream().map(mapping -> mapping.getContainerId()).collect(Collectors.toList()));

            ListCommonRequest req = constructListCommonRequest("id", containerId, "IN");
            Pair<Specification<Containers>, Pageable> pair = fetchData(req, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> containersList = containers.getContent();
            result.addAll(containersList);
        }

        if (request.getConsolidationId() != null) {
            ListCommonRequest req2 = constructListCommonRequest("consolidation_id", request.getConsolidationId(), "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(req2, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> containersList = containers.getContent();
            if (result.isEmpty()) {
                result.addAll(containersList);
            } else {
                result = result.stream().filter(result::contains).collect(Collectors.toList());
            }
        }

        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=\"containers.csv\"");

        try (PrintWriter writer = response.getWriter()) {
            writer.println(parser.generateCSVHeaderForContainer());
            for (Containers container : result) {
                writer.println(parser.formatContainerAsCSVLine(container));
            }
        }
    }

    @Override
    public void downloadContainerEvents(HttpServletResponse response, BulkDownloadRequest request) throws Exception {
        List<Events> result = new ArrayList<>();
        if (request.getConsolidationId() != null) {
            ListCommonRequest req2 = constructListRequestFromEntityId(Long.valueOf(request.getConsolidationId()), "CONSOLIDATION");
            Pair<Specification<Events>, Pageable> pair = fetchData(req2, Events.class);
            Page<Events> containerEventsPage = eventDao.findAll(pair.getLeft(), pair.getRight());
            List<Events> containerEvents = containerEventsPage.getContent();
            if (result.isEmpty()) {
                result.addAll(containerEvents);
            } else {
                result = result.stream().filter(result::contains).collect(Collectors.toList());
            }
        }

        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=\"consolidation_events.csv\"");

        try (PrintWriter writer = response.getWriter()) {
            writer.println(parser.generateCSVHeaderForEvent());
            for (Events event : result) {
                writer.println(parser.formatEventAsCSVLine(event));
            }
        }
    }


    @Transactional
    public ResponseEntity<?> attachPacks(Long containerId, List<Long> packsId) {
        Containers containers = containerDao.findById(containerId).get();

        if (containers != null) {
            for (Long packid : packsId) {
                Packing packing = packingDao.findById(packid).get();
                if (packing != null) {
                    containers.getPacksList().add(packing);
                }
            }
            Containers entity = containerDao.save(containers);
            afterSave(entity, false);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ContainerResponse.class));
        }

        return null;
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Containers> oldEntity = containerDao.findById(id);
        List<Long> updatedPackIds = new ArrayList<>();
        List<PackingRequest> updatedPackingRequest = new ArrayList<>();
        List<PackingRequest> packingRequestList = request.getPacksList();
        if(packingRequestList != null && !packingRequestList.isEmpty()) {
            for(PackingRequest packingRequest : packingRequestList) {
                if(packingRequest.getId() != null) {
                    updatedPackIds.add(packingRequest.getId());
                }
            }
        }

        List<PackingRequest> packingRequestWithEmptyContainerId = new ArrayList<>();
        if(packingRequestList != null && !packingRequestList.isEmpty()) {
            for(PackingRequest packingRequest : packingRequestList) {
                if(packingRequest.getContainerId() == null) {
                    packingRequestWithEmptyContainerId.add(packingRequest);
                }
            }
        }

        request.setPacksList(updatedPackingRequest);



        Containers containers = convertRequestToEntity(request);

        if(containers.getGuid() != null && !oldEntity.get().getGuid().equals(containers.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            containers = containerDao.save(containers);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(containers)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Containers.class))
                            .parent(Containers.class.getSimpleName())
                            .parentId(containers.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            if (packingRequestList != null) {
                packingDao.removeContainerFromPacking(convertToEntityList(packingRequestList, Packing.class), id, updatedPackIds);
                packingDao.insertContainerInPacking(convertToEntityList(packingRequestWithEmptyContainerId, Packing.class), id);

            }
            if(eventsRequestList != null){
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), containers.getId(), Constants.CONTAINER);
                containers.setEventsList(events);
            }
            afterSave(containers, false);
            log.info("Updated the container details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Containers List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentsList", request.getId(), "CONTAINS");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(pair.getLeft(), pair.getRight());

            log.info("Container detail list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containersPage.getContent()),
                    containersPage.getTotalPages(),
                    containersPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Containers async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Container detail async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(containersPage.getContent()),
                                    containersPage.getTotalPages(),
                                    containersPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        if (commonRequestModel == null) {
            log.debug("Request is empty for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();
        Optional<Containers> container = containerDao.findById(id);
        if (container.isEmpty()) {
            log.debug("Container details are null for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        try {
            packingDao.deleteEntityFromContainer(id);
            String oldEntityJsonString = jsonHelper.convertToJson(container.get());
            containerDao.delete(container.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Containers.class))
                            .parent(Containers.class.getSimpleName())
                            .parentId(container.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted container for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Containers> container = containerDao.findById(id);
            if (container.isEmpty()) {
                log.debug("Container is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Container detail fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            JobResponse response = (JobResponse) convertEntityToDto(container.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Containers changeAchievedUnit(Containers container) throws Exception{
        try {
            if(!IsStringNullOrEmpty(container.getAchievedVolumeUnit()) && !IsStringNullOrEmpty(container.getAllocatedVolumeUnit()) && !container.getAchievedVolumeUnit().equals(container.getAllocatedVolumeUnit())) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());
                container.setAchievedVolume(val);
                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
            }
            if(!IsStringNullOrEmpty(container.getAchievedWeightUnit()) && !IsStringNullOrEmpty(container.getAllocatedWeightUnit()) && !container.getAchievedWeightUnit().equals(container.getAllocatedWeightUnit())) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                container.setAchievedWeight(val);
                container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
            }
            return container;
        } catch (Exception e) {
            throw new Exception(e);
        }
    }

    @Override
    public ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerRequest containerRequest = (ContainerRequest) commonRequestModel.getData();
            Containers container = convertRequestToEntity(containerRequest);
            container = changeAchievedUnit(container);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateAchievedQuantity_onPackAssign(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerPackAssignDetachRequest request = (ContainerPackAssignDetachRequest) commonRequestModel.getData();

            Optional<Containers> containersOptional = containerDao.findById(request.getContainerId());
            if(containersOptional.isPresent()) {
                Containers container = containersOptional.get();
                changeAchievedUnit(container);
                ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getPacksId(), "IN");
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                if(!packings.isEmpty() && packings.get().findAny().isPresent()) {
                    List<Packing> packingList = packings.stream().toList();
                    for(Packing packing: packingList) {
                        if(packing.getWeight() != null && !packing.getWeightUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedWeightUnit())) {
                            BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                            container.setAchievedWeight(container.getAchievedWeight().add(val));
                            container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
                        }
                        if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedVolumeUnit())) {
                            BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
                            container.setAchievedVolume(container.getAchievedVolume().add(val));
                            container.setVolumeUtilization(((container.getAchievedVolume().divide(container.getAllocatedVolume())).multiply(new BigDecimal(100))).toString());
                        }
                    }
                    return assignContainers(packingList, container, request.getShipmentId());
                }
            }
            responseMsg = "Data not available for provided request";
            throw new DataRetrievalFailureException(responseMsg);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> assignContainers(List<Packing> packingList, Containers container, Long shipmentId) {
        String responseMsg;
        try {
            shipmentsContainersMappingDao.assignShipments(container.getId(), List.of(shipmentId));
            Containers containers = containerDao.save(jsonHelper.convertValue(container, Containers.class));
            for (Packing packing: packingList) {
                packing.setContainerId(container.getId());
            }
            packingDao.saveAll(packingList);
            afterSave(containers, false);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerPackAssignDetachRequest request = (ContainerPackAssignDetachRequest) commonRequestModel.getData();

            Optional<Containers> containersOptional = containerDao.findById(request.getContainerId());
            if(containersOptional.isPresent()) {
                Containers container = containersOptional.get();
                changeAchievedUnit(container);
                ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getPacksId(), "IN");
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                if(!packings.isEmpty() && packings.get().findAny().isPresent())
                {
                    List<Packing> packingList = packings.stream().toList();
                    for(Packing packing: packingList) {
                        if(packing.getWeight() != null && !packing.getWeightUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedWeightUnit())) {
                            BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                            container.setAchievedWeight(container.getAchievedWeight().subtract(val));
                            container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
                        }
                        if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedVolumeUnit())) {
                            BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
                            container.setAchievedVolume(container.getAchievedVolume().subtract(val));
                            container.setVolumeUtilization(((container.getAchievedVolume().divide(container.getAllocatedVolume())).multiply(new BigDecimal(100))).toString());
                        }
                    }
                    return detachContainer(packingList, container, request.getShipmentId());
                }
            }
            responseMsg = "Data not available for provided request";
            throw new DataRetrievalFailureException(responseMsg);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> detachContainer(List<Packing> packingList, Containers container, Long shipmentId) {
        String responseMsg;
        try {
            shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipmentId));
            Containers containers = containerDao.save(jsonHelper.convertValue(container, Containers.class));
            for (Packing packing: packingList) {
                packing.setContainerId(null);
            }
            packingDao.saveAll(packingList);
            afterSave(containers, false);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> getContainersForSelection(CommonRequestModel commonRequestModel) {
        String responseMsg;
        Boolean lclAndSeaOrRoadFlag = false; // TODO- Remove this and fetch from tenant Settings and Shipment Data
        Boolean IsConsolidatorFlag = true; // TODO- Remove this and fetch from tenant Settings
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignRequest containerAssignRequest = (ContainerAssignRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            if(lclAndSeaOrRoadFlag) {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(e -> e.getShipmentId()).collect(Collectors.toList()).contains(shipmentId)) {
                        if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null && container.getAchievedWeight() != null
                           && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {
                            BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                            BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());
                            if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                                containersList.add(container);
                            }
                        }
                    }
                }
            }
            else {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(e -> e.getShipmentId()).collect(Collectors.toList()).contains(shipmentId)) {
                        containersList.add(container);
                    }
                }
            }
            if(containerAssignRequest.getTake() != null)
                containers = new PageImpl<>(containersList.subList(0, Math.min(containerAssignRequest.getTake(), containersList.size())), PageRequest.of(0, containerAssignRequest.getTake()), containersList.size());
            else
                containers = new PageImpl<>(containersList);
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containers.getContent()),
                    containers.getTotalPages(),
                    containers.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public void afterSave(Containers containers, boolean isCreate) {
        try {
            KafkaResponse kafkaResponse = producer.getKafkaResponse(containers, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing container to kafka");
        }
    }

    public void afterSaveList(List<Containers> containers, boolean isCreate) {
        if(containers != null && containers.size() > 0) {
            for (Containers container : containers) {
                afterSave(container, isCreate);
            }
        }
    }

    @Override
    public ResponseEntity<?> V1ContainerCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception {
        ContainerRequestV2 containerRequest = (ContainerRequestV2) commonRequestModel.getData();
        try {
            List<Containers> existingCont = containerDao.findByGuid(containerRequest.getGuid());
            Containers containers = modelMapper.map(containerRequest, Containers.class);
            List<Long> shipIds = null;
            boolean isCreate = true;
            if(existingCont != null && existingCont.size() > 0) {
                containers.setId(existingCont.get(0).getId());
                containers.setConsolidationId(existingCont.get(0).getConsolidationId());
                isCreate = false;
            }
            else
            {
                if(containerRequest.getConsolidationGuid() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(containerRequest.getConsolidationGuid());
                    if(!consolidationDetails.isEmpty() && consolidationDetails.get() != null) {
                        containers.setConsolidationId(consolidationDetails.get().getId());
                    }
                }
                if(containerRequest.getShipmentGuids() != null && containerRequest.getShipmentGuids().size() > 0) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest("guid", containerRequest.getShipmentGuids(), "IN");
                    Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
                    Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
                    if(shipmentDetails.get() != null && shipmentDetails.get().count() > 0) {
                        shipIds = shipmentDetails.get().map(e -> e.getId()).collect(Collectors.toList());
                    }
                }
            }
            containers = containerDao.save(containers);
            afterSave(containers, isCreate);
            if(shipIds != null) {
                shipmentsContainersMappingDao.assignShipments(containers.getId(), shipIds);
            }
            ContainerResponse response = objectMapper.convertValue(containers, ContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    @Override
    public void exportContainers(HttpServletResponse response, ExportContainerListRequest request) throws Exception {
        List<ShipmentsContainersMapping> mappings;
        Optional<ConsolidationDetails> consol = null;
        List<IRunnerResponse> containersList = null;
        if (request.getConsolidationId() != null) {
            consol = consolidationDetailsDao.findById(Long.valueOf(request.getConsolidationId()));

            if (consol.isEmpty())
                throw new RuntimeException("Consolidation does not exist, pls save the consol first");

            if (consol.get().getContainersList().isEmpty())
                throw new RuntimeException("No containers found attached to consoliation");

            List<Containers> containers = consol.get().getContainersList();
            if (containers == null || containers.isEmpty()) {
                throw new RuntimeException("No containers present for this consol");
            }
            containersList = convertEntityListToDtoList(containers);

        } else {
            throw new RuntimeException("Consolidation does not exist, pls save the consol first");
        }

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("ContainersList");
        makeHeadersInSheet(sheet, consol);

        for (int i = 0; i < containersList.size(); i++) {
            Row itemRow = sheet.createRow(i + 1);
            ContainerResponse container = (ContainerResponse) containersList.get(i);
            var consolBasicValues = parser.getAllAttributeValuesAsListContainer(container);
            int offset = 0;
            for (int j = 0; j < consolBasicValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(consolBasicValues.get(j));
            offset += consolBasicValues.size();

            itemRow.createCell(offset + 0).setCellValue(consol.get().getBol());
            itemRow.createCell(offset + 1).setCellValue(0);
            itemRow.createCell(offset + 2).setCellValue(0);
            itemRow.createCell(offset + 3).setCellValue(request.getFreeTimeNoOfDaysDetention());
            itemRow.createCell(offset + 4).setCellValue(request.getFreeTimeNoOfDaysStorage());
            itemRow.createCell(offset + 5).setCellValue(consol.get().getCarrierDetails().getVoyage());

            var booking = customerBookingDao.findById(container.getBookingId());
            var bookingNum = booking.isPresent() ? booking.get().getBookingNumber() : "";
            var bookingDate = booking.isPresent() ? booking.get().getBookingDate() : null;

            itemRow.createCell(offset + 6).setCellValue(bookingDate);
            itemRow.createCell(offset + 7).setCellValue(bookingNum);
        }

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=containerList.xlsx");

        try (OutputStream outputStream = response.getOutputStream()) {
            workbook.write(outputStream);
        }

    }

    private void makeHeadersInSheet(Sheet sheet, Optional<ConsolidationDetails> consol) {
//        Row preHeaderRow = sheet.createRow(0);
        Row headerRow = sheet.createRow(0);
        List<String> containerHeader = parser.getHeadersForContainer();
        for (int i = 0; i < containerHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(containerHeader.get(i));
        }

        containerHeader.add("bol");
        containerHeader.add("NoOfDays (Detention)");
        containerHeader.add("NoOfDays (Storage)");
        containerHeader.add("FreeTimeNoOfDays (Storage)");
        containerHeader.add("FreeTimeNoOfDays (Detention)");
        containerHeader.add("Voyage");
        containerHeader.add("Booking Date");
        containerHeader.add("Booking Number");
    }


    private IRunnerResponse convertEntityToDto(Containers container) {
        return jsonHelper.convertValue(container, ContainerResponse.class);
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return jsonHelper.convertValue(request, Containers.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> {
            responseList.add(convertEntityToDto(containers));
        });
        return responseList;
    }

}
