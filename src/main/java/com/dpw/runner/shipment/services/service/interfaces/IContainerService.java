package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.List;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IContainerService extends ICommonService {
    ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);
    ResponseEntity<?> calculateAchievedQuantity_onPackAssign(CommonRequestModel commonRequestModel);
    ResponseEntity<?> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel);
    ResponseEntity<?> getContainersForSelection(CommonRequestModel commonRequestModel);
    void uploadContainers(MultipartFile file) throws Exception;

    void downloadContainers(HttpServletResponse response) throws Exception;
}
