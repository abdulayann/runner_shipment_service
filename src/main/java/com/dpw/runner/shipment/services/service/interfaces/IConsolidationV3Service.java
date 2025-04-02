package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;

public interface IConsolidationV3Service {

    ShipmentGridChangeResponse calculateAchievedValues(Long consolidationId) throws RunnerException;

    ConsolidationDetailsResponse create(ConsolidationDetailsRequest request);
    ConsolidationDetailsResponse createConsolidationForBooking(CommonRequestModel commonRequestModel);
    ConsolidationDetailsResponse createConsolidationFromEntityTransfer(ConsolidationDetailsRequest request);
    ConsolidationDetailsResponse completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;
    ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException;
    void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException;
    String attachShipments(ShipmentAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;
    void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes, boolean reverseSyncFromConsolToShipment) throws RunnerException;
    void checkSciForAttachConsole(Long consoleId) throws RunnerException;
    void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, ConsolidationDetails oldEntity);
    ConsolidationDetailsResponse retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData) throws RunnerException;
    Map<String, Object> getAllMasterData(CommonRequestModel commonRequestModel);
}