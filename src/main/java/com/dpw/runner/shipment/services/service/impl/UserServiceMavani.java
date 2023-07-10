package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.stereotype.Service;
import java.util.Arrays;

@Service
public class UserServiceMavani implements IUserService {
    private static UsersDto usersDto = new UsersDto(1, 2, "userName", "displayName",
            "email@dpworld.com", 1, 1, Arrays.asList("air-exp-all-shipmentList", "air-exp-lcl-shipmentList",
            "air-imp-fcl-shipmentList", "sea-shipmentList", "sea-exp-fcl-shipmentList", "sea-exp-lcl-shipmentList", "sea-imp-fcl-shipmentList",
            "sea-imp-lcl-shipmentList", "air-shipmentList"));

    @Override
    public UsersDto getUserByUserName(String userName) {
        return usersDto;
    } // TODO- actually fetch from mavani
}
