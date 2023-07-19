package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface IContainerService extends ICommonService {
    void uploadContainers(MultipartFile file) throws Exception;

    void downloadContainers(HttpServletResponse response) throws Exception;
}
