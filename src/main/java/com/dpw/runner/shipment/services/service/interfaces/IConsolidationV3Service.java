package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeResponse;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;
import java.util.Map;

public interface IConsolidationV3Service {
    ShipmentGridChangeResponse calculateAchievedValues(Long consolidationId) throws RunnerException;
    ConsolidationDetailsResponse create(ConsolidationDetailsRequest request);
    ConsolidationDetailsResponse createConsolidationForBooking(CommonRequestModel commonRequestModel);
    ConsolidationDetailsResponse createConsolidationFromEntityTransfer(ConsolidationDetailsRequest request);
    ConsolidationDetailsResponse completeUpdate(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException;
    ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException;
    void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException;
    String attachShipments(ShipmentAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;

    void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes) throws RunnerException;
    void checkSciForAttachConsole(Long consoleId) throws RunnerException;
    void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, ConsolidationDetails oldEntity);
    ConsolidationDetailsV3Response retrieveById(CommonGetRequest commonGetRequest, boolean getMasterData) throws RunnerException;
    Map<String, Object> getAllMasterData(CommonGetRequest commonGetRequest);
}