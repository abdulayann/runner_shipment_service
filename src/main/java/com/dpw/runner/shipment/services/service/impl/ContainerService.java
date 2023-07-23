package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
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
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@Slf4j
@Service
public class ContainerService implements IContainerService {

    @Autowired
    IContainerDao containerDao;
    @Autowired
    ModelMapper modelMapper;
    @Autowired
    IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Autowired
    IEventDao eventDao;
    @Autowired
    private IPackingDao packingDao;

    @Autowired
    UnitConversionService unitConversionService;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if(request == null) {
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
            if(request.getShipmentIds() != null) {
                shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), request.getShipmentIds());
            }
            if(eventsRequestList != null){
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), container.getId(), Constants.CONTAINER);
                container.setEventsList(events);
            }
            log.info("Container Details Saved Successfully for Id {} with Request Id {}", container.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Containers> oldEntity = containerDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Container is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Containers containers = convertRequestToEntity(request);
        List<EventsRequest> eventsRequestList = request.getEventsList();
        containers.setId(oldEntity.get().getId());
        try {
            containers = containerDao.save(containers);
            List<PackingRequest> packingRequestList = request.getPacksList();
            List<Packing> oldPackings = oldEntity.get().getPacksList();
            if(request.getShipmentIds() != null) {
                shipmentsContainersMappingDao.updateShipmentsMappings(containers.getId(), request.getShipmentIds());
            }
            if (packingRequestList != null) {
                packingDao.updateEntityFromContainer(convertToEntityList(packingRequestList, Packing.class), id);
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
            if(request == null) {
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Containers async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage  = containerDao.findAll(tuple.getLeft(), tuple.getRight());
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
        if(commonRequestModel == null) {
            log.debug("Request is empty for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if(commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();
        Optional<Containers> container = containerDao.findById(id);
        if (container.isEmpty()) {
            log.debug("Container details are null for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        try {
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
            if(request == null) {
                log.error("Request is empty for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
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
                BigDecimal val = new BigDecimal(unitConversionService.convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());
                container.setAchievedVolume(val);
                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
            }
            if(container.getAchievedWeightUnit() != container.getAllocatedWeightUnit()) {
                BigDecimal val = new BigDecimal(unitConversionService.convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
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
                    BigDecimal val = new BigDecimal(unitConversionService.convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                    container.setAchievedWeight(container.getAchievedWeight().add(val));
                    container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
                }
                if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty()) {
                    BigDecimal val = new BigDecimal(unitConversionService.convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
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
                    BigDecimal val = new BigDecimal(unitConversionService.convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                    container.setAchievedWeight(container.getAchievedWeight().subtract(val));
                    container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
                }
                if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty()) {
                    BigDecimal val = new BigDecimal(unitConversionService.convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
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
                        if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedWeight().compareTo(container.getAchievedWeight()) > 0 &&
                                container.getAllocatedVolume() != null && container.getAchievedWeight() != null && container.getAllocatedVolume().compareTo(container.getAchievedVolume()) > 0) {
                            containersList.add(container);
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
        return modelMapper.map(container, ContainerResponse.class);
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return modelMapper.map(request, Containers.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> {
            responseList.add(convertEntityToDto(containers));
        });
        return responseList;
    }

}
