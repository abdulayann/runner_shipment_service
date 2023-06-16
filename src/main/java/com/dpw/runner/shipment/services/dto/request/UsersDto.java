package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import java.util.List;


@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class UsersDto {

    private Integer Id;

    private Integer UserId;

    private String UserName;

    private String DisplayName;

    private String Email;

    private Integer IsActive;

    private Integer TenantId;

    private List<String> Permissions;

}
