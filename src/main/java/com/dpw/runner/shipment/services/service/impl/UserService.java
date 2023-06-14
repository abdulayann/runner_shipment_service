package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.UsersDto;
import com.dpw.runner.shipment.services.service.IUserService;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

@Service
public class UserService implements IUserService {
    private static UsersDto usersDto = new UsersDto(1,2,"userName","displayName",
                                                "email@dpworld.com",1,1, Arrays.asList("airexportfclshipmentList","airexportlclshipmentList",
            "airimportfclshipmentlist","airimportlclshipmentlist","seaexportfclshipmentList","seaexportlclshipmentList","seaimportfclshipmentlist",
            "seaimportlclshipmentlist"));
    @Override
    public UsersDto getUserByUserName(String userName) {
        return usersDto;
    }
}
