package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ContainerPackAssignDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToEntityList;
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
    private final CSVParsingUtil<Containers> parser = new CSVParsingUtil<>(Containers.class);

    @Autowired
    IEventDao eventDao;
    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private AuditLogService auditLogService;

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
            writer.println(parser.generateCSVHeader());
            for (Containers container : result) {
                writer.println(parser.formatContainerAsCSVLine(container));
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
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {

            containers = containerDao.save(containers);
            if (packingRequestList != null) {
                packingDao.removeContainerFromPacking(convertToEntityList(packingRequestList, Packing.class), id, updatedPackIds);
                packingDao.insertContainerInPacking(convertToEntityList(packingRequestWithEmptyContainerId, Packing.class), id);

            }
            if(eventsRequestList != null){
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), containers.getId(), Constants.CONTAINER);
                containers.setEventsList(events);
            }
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
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Containers List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
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
            containerDao.delete(container.get());
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
            if(container.getAchievedVolumeUnit() != container.getAllocatedVolumeUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());
                container.setAchievedVolume(val);
                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
            }
            if(container.getAchievedWeightUnit() != container.getAllocatedWeightUnit()) {
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
            ContainerPackAssignDetachRequest containerPackAssignDetachRequest = (ContainerPackAssignDetachRequest) commonRequestModel.getData();
            List<Packing> packingList = convertToEntityList(containerPackAssignDetachRequest.getPackingRequestList(), Packing.class);
            Containers container = convertRequestToEntity(containerPackAssignDetachRequest.getContainer());
            container = changeAchievedUnit(container);
            for(Packing packing: packingList) {
                if(packing.getWeight() != null && !packing.getWeightUnit().isEmpty()) {
                    BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                    container.setAchievedWeight(container.getAchievedWeight().add(val));
                    container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
                }
                if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty()) {
                    BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
                    container.setAchievedVolume(container.getAchievedVolume().add(val));
                    container.setVolumeUtilization(((container.getAchievedVolume().divide(container.getAllocatedVolume())).multiply(new BigDecimal(100))).toString());
                }
            }
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerPackAssignDetachRequest containerPackAssignDetachRequest = (ContainerPackAssignDetachRequest) commonRequestModel.getData();
            List<Packing> packingList = convertToEntityList(containerPackAssignDetachRequest.getPackingRequestList(), Packing.class);
            Containers container = convertRequestToEntity(containerPackAssignDetachRequest.getContainer());
            container = changeAchievedUnit(container);
            for(Packing packing: packingList) {
                if(packing.getWeight() != null && !packing.getWeightUnit().isEmpty()) {
                    BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                    container.setAchievedWeight(container.getAchievedWeight().subtract(val));
                    container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
                }
                if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty()) {
                    BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
                    container.setAchievedVolume(container.getAchievedVolume().subtract(val));
                    container.setVolumeUtilization(((container.getAchievedVolume().divide(container.getAllocatedVolume())).multiply(new BigDecimal(100))).toString());
                }
            }
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> getContainersForSelection(CommonRequestModel commonRequestModel) {
        String responseMsg;
        Boolean lclAndSeaOrRoadFlag = true; // TODO- Remove this and fetch from tenant Settings and Shipment Data
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
