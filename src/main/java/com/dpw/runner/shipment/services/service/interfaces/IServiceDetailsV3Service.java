package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkServiceDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;
import java.util.Map;

public interface IServiceDetailsV3Service {

    ServiceDetailsResponse create(ServiceDetailsRequest serviceDetailsRequest, String module) throws RunnerException;

    String delete(Long id, String module) throws RunnerException;

    BulkServiceDetailsResponse updateBulk(List<ServiceDetailsRequest> request, String module) throws RunnerException;

    BulkServiceDetailsResponse deleteBulk(List<ServiceDetailsRequest> request, String module) throws RunnerException;

    ServiceDetailsResponse retrieveById(Long id, String guid, String xSource);

    ServiceDetailsListResponse list(ListCommonRequest listCommonRequest, boolean getMasterData, String xSource);

    ServiceDetailsListResponse fetchShipmentServices(ListCommonRequest commonRequestModel, String xSource);

    Map<String, Object> getAllMasterData(Long id, String xSource);

    Map<String, Object> fetchAllMasterDataByKey(ServiceDetailsResponse serviceDetailsResponse);
}
