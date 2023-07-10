package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentSettingsService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;


import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ShipmentSettingsService implements IShipmentSettingsService {

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingRequest request = null;
        request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Shipment Settings create");
        }
        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        try {
            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);
            log.info("Shipment Setting Details created successfully for Id {}", shipmentSettingsDetails.getId());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingRequest request = (ShipmentSettingRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Shipment Settings update");
        }

        if(request.getId() == null) {
            log.error("Request Id is null for Shipment Settings update");
        }
        long id = request.getId();
        Optional<ShipmentSettingsDetails> oldEntity = shipmentSettingsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Shipment Setting is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentSettingsDetails shipmentSettingsDetails = convertRequestToEntity(request);
        shipmentSettingsDetails.setId(oldEntity.get().getId());
        try {
            shipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);
            log.info("Updated the Shipment Setting details for Id {} ", id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentSettingsDetails));
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Shipment Settings retrieve");
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Shipment Settings retrieve");
            }
            long id = request.getId();
            Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.findById(id);
            if(!shipmentSettingsDetails.isPresent()) {
                log.debug("Shipment Setting is null for Id {}", id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment Settings details fetched successfully for Id {}", id);
            ShipmentSettingsDetailsResponse response = convertEntityToDto(shipmentSettingsDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Shipment Settings list");
            }
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(request, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            log.info("Shipment Settings list retrieved successfully");
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentSettingsPage.getContent()),
                    shipmentSettingsPage.getTotalPages(),
                    shipmentSettingsPage.getTotalElements());
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
                log.error("Request is empty for Shipment Settings async list");
            }
            Pair<Specification<ShipmentSettingsDetails>, Pageable> tuple = fetchData(request, ShipmentSettingsDetails.class);
            Page<ShipmentSettingsDetails> shipmentSettingsPage  = shipmentSettingsDao.list(tuple.getLeft(), tuple.getRight());
            log.info("Shipment Settings list retrieved successfully");
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentSettingsPage.getContent()),
                    shipmentSettingsPage.getTotalPages(),
                    shipmentSettingsPage.getTotalElements()));
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
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Shipment Settings delete");
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Shipment Settings delete");
            }
            long id = request.getId();
            Optional<ShipmentSettingsDetails> note = shipmentSettingsDao.findById(id);
            if (note.isEmpty()) {
                log.debug("ShipmentSettingsDetails is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentSettingsDao.delete(note.get());
            log.info("Deleted Shipment Settings for Id {}", id);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
    public ShipmentSettingsDetails convertRequestToEntity(ShipmentSettingRequest request) {
        return modelMapper.map(request, ShipmentSettingsDetails.class);
    }

    private ShipmentSettingsDetailsResponse convertEntityToDto(ShipmentSettingsDetails shipmentSettingsDetails) {
        return modelMapper.map(shipmentSettingsDetails, ShipmentSettingsDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentSettingsDetails> list) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        list.forEach(shipmentSettingsDetail -> {
            responseList.add(convertEntityToDto(shipmentSettingsDetail));
        });
        return responseList;
    }
}
