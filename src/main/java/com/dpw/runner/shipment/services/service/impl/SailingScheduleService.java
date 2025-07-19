package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ISailingScheduleDao;
import com.dpw.runner.shipment.services.dto.request.SailingScheduleRequest;
import com.dpw.runner.shipment.services.entity.SailingSchedule;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISailingScheduleService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class SailingScheduleService implements ISailingScheduleService {

    private ISailingScheduleDao sailingScheduleDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    public SailingScheduleService(ISailingScheduleDao sailingScheduleDao) {
        this.sailingScheduleDao = sailingScheduleDao;
    }

    @Override
    public ResponseEntity<IRunnerResponse> create(List<SailingScheduleRequest> requestList) {
        return ResponseHelper.buildSuccessResponse(sailingScheduleDao.saveAll(jsonHelper.convertValueToList(requestList, SailingSchedule.class)));
    }
}
