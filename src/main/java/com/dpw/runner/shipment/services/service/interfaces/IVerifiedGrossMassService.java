package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SubmitAmendInttraRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassBulkUpdateRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.kafka.dto.inttra.VgmEventDto;
import org.springframework.http.ResponseEntity;
import java.util.List;

public interface IVerifiedGrossMassService {

    /**
     * Create a new Verified Gross mass.
     *
     * @param request VerifiedGrossMassRequest
     * @return VerifiedGrossMassResponse
     */
    VerifiedGrossMassResponse create(VerifiedGrossMassRequest request);

    /**
     * Retrieve a Verified Gross mass by its ID.
     *
     * @param id booking id
     * @return VerifiedGrossMassResponse
     */
    VerifiedGrossMassResponse retrieveById(Long id);

    ResponseEntity<IRunnerResponse> list(CommonRequestModel listCommonRequest, boolean getMasterData);

    /**
     * Update an existing Verified Gross mass.
     *
     *
     * @param request VerifiedGrossMassRequest
     * @return VerifiedGrossMassResponse
     */
    VerifiedGrossMassResponse update(VerifiedGrossMassRequest request);

    /**
     * Delete a Verified Gross mass by its ID.
     *
     * @param id booking id
     */
    void delete(Long id);

    ResponseEntity<IRunnerResponse> getAllMasterData(Long vgmId);

    VerifiedGrossMassResponse getDefaultVerifiedGrossMassValues(EntityType type, Long entityId);

    List<CommonContainerResponse> bulkUpdateContainers(VerifiedGrossMassBulkUpdateRequest request);

    /**
     * Submit/Amend Verified Gross mass.
     *
     * @param submitAmendInttraRequest request
     *
     */
    void submitOrAmendVerifiedGrossMass(SubmitAmendInttraRequest submitAmendInttraRequest) throws RunnerException;

    void updateVgmStatus(VgmEventDto vgm);
}

