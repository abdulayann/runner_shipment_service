package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentServiceRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentServiceResponse;
import com.dpw.runner.shipment.services.entity.ShipmentServices;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentServicesDao;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServicesService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentServicesService implements IShipmentServicesService {
    @Autowired
    private IShipmentServicesDao shipmentServicesDao;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        ShipmentServiceRequest request = null;
        request = (ShipmentServiceRequest) commonRequestModel.getData();
        ShipmentServices shipmentServices = convertRequestToEntity(request);
        shipmentServices = shipmentServicesDao.save(shipmentServices);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ShipmentServiceRequest request = (ShipmentServiceRequest) commonRequestModel.getData();
        long id =request.getId();
        Optional<ShipmentServices> oldEntity = shipmentServicesDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Shipment Service is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentServices shipmentServices = convertRequestToEntity(request);
        shipmentServices.setId(oldEntity.get().getId());
        shipmentServices = shipmentServicesDao.save(shipmentServices);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(shipmentServices));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<ShipmentServices>, Pageable> tuple = fetchData(request, ShipmentServices.class);
            Page<ShipmentServices> shipmentServicesPage  = shipmentServicesDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentServicesPage.getContent()),
                    shipmentServicesPage.getTotalPages(),
                    shipmentServicesPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<ShipmentServices> shipmentServices = shipmentServicesDao.findById(id);
            if(!shipmentServices.isPresent()) {
                log.debug("Booking Carriage is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentServicesDao.delete(shipmentServices.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<ShipmentServices> shipmentServices = shipmentServicesDao.findById(id);
            if(!shipmentServices.isPresent()) {
                log.debug("Booking Carriage is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ShipmentServiceResponse response = convertEntityToDto(shipmentServices.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private static ShipmentServiceResponse convertEntityToDto(ShipmentServices shipmentServices) {
        ShipmentServiceResponse response = new ShipmentServiceResponse();
        response.setId(shipmentServices.getId());
        response.setGuid(shipmentServices.getGuid());
        response.setShipmentId(shipmentServices.getShipmentId());
        response.setConsolidationId(shipmentServices.getConsolidationId());
        response.setServiceType(shipmentServices.getServiceType());
        response.setContractorId(shipmentServices.getContractorId());
        response.setContractorAddressId(shipmentServices.getContractorAddressId());
        response.setSrvLocation(shipmentServices.getSrvLocation());
        response.setBookingDate(shipmentServices.getBookingDate());
        response.setServiceCount(shipmentServices.getServiceCount());
        response.setServiceDuration(shipmentServices.getServiceDuration());
        response.setCompletionDate(shipmentServices.getCompletionDate());
        response.setRefNumber(shipmentServices.getRefNumber());
        response.setServiceNotes(shipmentServices.getServiceNotes());
        return response;
    }

    private static List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentServices> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentServices -> {
            responseList.add(convertEntityToDto(shipmentServices));
        });
        return responseList;
    }

    public static ShipmentServices convertRequestToEntity(ShipmentServiceRequest request) {
        ShipmentServices shipmentServices = new ShipmentServices();
        shipmentServices.setGuid(UUID.randomUUID());
        shipmentServices.setShipmentId(request.getShipmentId());
        shipmentServices.setConsolidationId(request.getConsolidationId());
        shipmentServices.setServiceType(request.getServiceType());
        shipmentServices.setContractorId(request.getContractorId());
        shipmentServices.setContractorAddressId(request.getContractorAddressId());
        shipmentServices.setSrvLocation(request.getSrvLocation());
        shipmentServices.setBookingDate(request.getBookingDate());
        shipmentServices.setServiceCount(request.getServiceCount());
        shipmentServices.setServiceDuration(request.getServiceDuration());
        shipmentServices.setCompletionDate(request.getCompletionDate());
        shipmentServices.setRefNumber(request.getRefNumber());
        shipmentServices.setServiceNotes(request.getServiceNotes());
        return shipmentServices;
    }
}
