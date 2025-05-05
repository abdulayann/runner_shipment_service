package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingListResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface IPackingV3Service {
    PackingResponse create(PackingV3Request packingRequest, String module) throws RunnerException;
    PackingResponse update(PackingV3Request packingRequest, String module) throws RunnerException;
    String delete(Long id, String module) throws RunnerException;
    BulkPackingResponse updateBulk(List<PackingV3Request> request, String module) throws RunnerException;
    BulkPackingResponse deleteBulk(List<PackingV3Request> request, String module);
    void downloadPacking(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException;
    PackingResponse retrieveById(CommonRequestModel commonRequestModel);
    RunnerListResponse<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData);

    PackingListResponse fetchShipmentPackages(CommonRequestModel commonRequestModel);
}
