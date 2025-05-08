package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingListResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;
import java.util.Map;


public interface IRoutingsV3Service{

    RoutingsResponse create(CommonRequestModel commonRequestModel, String module) throws RunnerException;

    RoutingsResponse update(CommonRequestModel commonRequestModel, String module) throws RunnerException;

    RoutingListResponse list(CommonRequestModel commonRequestModel, String xSource) throws RunnerException;

    void delete(CommonRequestModel commonRequestModel, String module) throws RunnerException;

    RoutingsResponse retrieveById(CommonRequestModel commonRequestModel, String xSource) throws RunnerException;
    BulkRoutingResponse updateBulk(List<RoutingsRequest> request, String module) throws RunnerException;
    BulkRoutingResponse deleteBulk(List<RoutingsRequest> request, String module) throws RunnerException;
    Map<String, Object> getAllMasterData(CommonRequestModel commonRequestModel, String xSource);

    List<Routings> getRoutingsByShipmentId(Long id);
}
