package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryV3Response;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignPackageContainerRequest;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.auth.AuthenticationException;
import org.springframework.web.bind.annotation.ModelAttribute;

public interface IPackingV3Service {

    PackingResponse create(PackingV3Request packingRequest, String module) throws RunnerException;

    PackingResponse update(PackingV3Request packingRequest, String module) throws RunnerException;

    String delete(Long id, String module) throws RunnerException;

    BulkPackingResponse updateBulk(List<PackingV3Request> request, String module) throws RunnerException;

    BulkPackingResponse deleteBulk(List<PackingV3Request> request, String module) throws RunnerException;

    void downloadPacking(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException;

    PackingResponse retrieveById(Long id, String guid, String xSource);

    PackingListResponse list(ListCommonRequest listCommonRequest, boolean getMasterData, String xSource);


    List<PackingResponse> fetchPacksAttachedToContainers(List<Long> containerIds);

    void removeContainersFromPacking(List<Long> containerIds);

    PackingListResponse fetchShipmentPackages(ListCommonRequest commonRequestModel, String xSource);

    PackingListResponse fetchConsolidationPackages(ListCommonRequest commonRequestModel, String xSource);

    PackSummaryV3Response calculatePackSummary(CalculatePackSummaryRequest calculatePackSummaryRequest, String xSource) throws AuthenticationException, RunnerException;

    Map<String, Object> getAllMasterData(Long id, String xSource);

    Map<String, Object> fetchAllMasterDataByKey(PackingResponse packingResponse);

    List<Long> filterContainerIdsAttachedToPacking(List<Long> containerIds);

    void processPacksAfterShipmentAttachment(Long consolidationId, ShipmentDetails shipmentDetails);

    Map<Long, ContainerInfoProjection> getContainerIdNumberMap(Set<Long> containerIds);
    ContainerResponse assignPackagesContainers(AssignContainerRequest request) throws RunnerException;
    void unAssignPackageContainers(UnAssignPackageContainerRequest request) throws RunnerException;
}
