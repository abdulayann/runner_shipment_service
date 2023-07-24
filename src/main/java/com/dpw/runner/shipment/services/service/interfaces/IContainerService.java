package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;

import javax.servlet.http.HttpServletResponse;

public interface IContainerService extends ICommonService {
    void uploadContainers(BulkUploadRequest request) throws Exception;

    void downloadContainers(HttpServletResponse response) throws Exception;
}
