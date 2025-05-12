package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.response.ConsolidationPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.util.List;
import java.util.Map;
import javax.validation.Valid;
import org.apache.http.auth.AuthenticationException;

public interface IConsolidationV3Service {
    ShipmentGridChangeV3Response calculateAchievedValues(Long consolidationId) throws RunnerException;
    ConsolidationDetailsResponse create(ConsolidationDetailsV3Request request);
    ConsolidationDetailsResponse createConsolidationForBooking(CommonRequestModel commonRequestModel);
    ConsolidationDetailsResponse completeUpdate(ConsolidationDetailsV3Request consolidationDetailsRequest) throws RunnerException;
    void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException;
    String attachShipments(ShipmentConsoleAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;

    void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails) throws RunnerException;
    void checkSciForAttachConsole(Long consoleId) throws RunnerException;
    void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, ConsolidationDetails oldEntity);
    ConsolidationDetailsV3Response retrieveById(CommonGetRequest commonGetRequest, String source) throws RunnerException, AuthenticationException;
    Map<String, Object> getAllMasterData(Long id, String source) throws RunnerException, AuthenticationException;
    ConsolidationPendingNotificationResponse getPendingNotificationData(CommonGetRequest request);
    ConsolidationListV3Response list(ListCommonRequest listCommonRequest, boolean getMasterData);
    ConsolidationListV3Response getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel);
    String detachShipments(@Valid ShipmentConsoleAttachDetachV3Request request)
        throws RunnerException;

    ConsolidationDetails getConsolidationById(Long consolidationId);
}