package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.UsersDto;
import com.dpw.runner.shipment.services.service.IUserService;
import org.springframework.stereotype.Service;

import java.util.Arrays;

@Service
public class UserService implements IUserService {
    private static UsersDto usersDto = new UsersDto(1, 2, "userName", "displayName",
            "email@dpworld.com", 1, 1, Arrays.asList("air-export-all-shipmentList", "air-export-lcl-shipmentList",
            "air-import-fcl-shipmentList", "sea-shipmentList", "sea-export-fcl-shipmentList", "sea-export-lcl-shipmentList", "sea-import-fcl-shipmentList",
            "sea-import-lcl-shipmentList"));

    @Override
    public UsersDto getUserByUserName(String userName) {
        return usersDto;
    }
}
