package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.stereotype.Service;
import java.util.Arrays;

@Service
public class UserServiceV1 implements IUserService {

    private static UsersDto usersDto = new UsersDto(1,2,"userName","displayName",
            "email@dpworld.com",1,2, Arrays.asList("airexportfclshipmentList","airexportlclshipmentList",
            "airimportfclshipmentlist","airimportlclshipmentlist","seaexportfclshipmentList","seaexportlclshipmentList","seaimportfclshipmentlist",
            "seaimportlclshipmentlist"));

    @Override
    public UsersDto getUserByUserName(String userName) {
        return usersDto;
    } // TODO- actually fetch from v1

}
