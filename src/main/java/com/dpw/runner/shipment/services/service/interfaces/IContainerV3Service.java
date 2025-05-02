package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface IContainerV3Service {
    ContainerResponse create(ContainerV3Request containerRequest);

    BulkContainerResponse updateBulk(List<ContainerRequest> request);

    BulkContainerResponse deleteBulk(List<ContainerRequest> request);

    ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId) throws RunnerException;
    ContainerListResponse fetchShipmentContainers(CommonRequestModel commonRequestModel);
    ContainerNumberCheckResponse validateContainerNumber(String containerNumber);
    void downloadContainers(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException;
    ContainerListResponse list(ListCommonRequest listCommonRequest, boolean isMasterData) throws RunnerException;
}
